#!/usr/bin/env python3

# Copyright 2020-2021 Seemant Kulleen <seemantk@gmail.com>

import os, boto3
import argparse
from cfn_tools import load_yaml, dump_yaml, dump_json
from jinja2 import Template

#
# Set Global Variables
#
imports = {}


#
# Helper functions
#

# Read raw yaml file and process as a Jinja template
def process_jinja_template(filename, kwargs):
    with open(filename) as f:
        contents = f.read()

    template = Template(contents)

    return load_yaml(template.render(**kwargs))

# Assemble all the components of a stack into a single cloudformation::stack object
def assemble(stack, kwargs, importing=False):
    # Create a dictionary to hold all the information
    stackobj = { 'AWSTemplateFormatVersion': '2010-09-09' }

    # Section names start with '0'
    sections = [l for l in os.listdir(stack) if l.startswith('0')]
    sections.sort()

    for section in sections:
        sec = section.split('_')[1]

        # Create an empty dictionary key for this section for all the files to add their data
        stackobj[sec] = {}

        for dirpath, dirs, files in os.walk(os.path.join(stack, section)):
            for f in files: # only read yml or txt files
                if not (f.endswith('.yml') or f.endswith('txt')): continue

                print("Flipping %s" % os.path.relpath(os.path.join(dirpath, f), '.'))
                # process each file as a jinja template first
                contents = process_jinja_template(os.path.join(dirpath, f), kwargs)

                try: # after jinja, we have pure yaml
                    if sec == 'Resources' and importing:
                        keys = [key for key in contents.keys() if key in imports.keys()]
                    else:
                        keys = contents.keys()

                    for key in keys:
                        # append each yaml object to the stackobj
                        if contents[key]:
                            stackobj[sec][key] = contents[key]
                except AttributeError: # this was a txt file, not yaml
                    # place the text into the stackobj
                    stackobj[sec] = contents

    return {k: v for k, v in stackobj.items() if v} # discard null/empty keys


def importify(resource, obj):
    obj['DeletionPolicy'] = 'Retain'

    temp = {}
    try:
        for prop in imports[resource]['Property']:
            print('Importing %s for %s' % (prop, resource))
            temp[prop] = obj['Properties'][prop]
    except KeyError: # e.g. SNSTopic requires TopicARN for import, but it is not a property in the template
        pass

    # Replace the Properties key with only the filtered items
    obj['Properties'] = temp

    return { 'ResourceType': obj['Type'], 'LogicalResourceId': resource, 'ResourceIdentifier': imports[resource]['Matcher'] }


def parse_command_line_arguments():
    class ParseKwargs(argparse.Action):
        def __call__(self, parser, namespace, values, option_string=None):
            setattr(namespace, self.dest, dict())
            for value in values:
                key, value = value.split('=')
                getattr(namespace, self.dest)[key.upper()] = value


    # Available command line arguments and their defaults
    parser = argparse.ArgumentParser(description='process jinja yaml files and assemble into a CloudFormation template')
    parser.add_argument('directory', metavar='dir', type=str, help='source directory')
    parser.add_argument('--region', dest='region', type=str, help='AWS Region')
    parser.add_argument('--output', dest='outfile', type=str, help='output CloudFormation YAML file')
    parser.add_argument('--import', dest='imports', type=str, help='import existing resources into the stack')
    parser.add_argument('--kwargs', dest='kwargs', nargs='*', action=ParseKwargs, help="list of KEY=value pairs")

    # return the parsed command line arguments
    return parser.parse_args()

#
# Main loop
#
def main():
    global imports

    args = parse_command_line_arguments()

    # Find AWS metadata for this stack deployment
    ec2   = boto3.setup_default_session(region_name=args.region)
    ec2   = boto3.client('ec2')
    zones = ec2.describe_availability_zones()['AvailabilityZones']

    args.kwargs['REGION']  = args.region
    args.kwargs['AZs']     = [z['ZoneName'] for z in zones]
    args.kwargs['AZcodes'] = [z.split('-')[2] for z in args.kwargs['AZs']]

    if args.imports: # process the ImportedResources file
        imports = process_jinja_template('ImportedResources.yml', args.kwargs)

    # Assemble the stack into a dict and convert that to yaml
    stack = assemble(args.directory, args.kwargs, importing=args.imports)

    if args.imports: # trim the stack based on the ImportedResources
        stack.pop('Outputs', None)
        imports_list = [importify(res, stack['Resources'][res]) for res in stack['Resources']]

        print(dump_json(imports_list))
        with open(args.imports, 'w') as f:
            f.write(dump_json(imports_list))

    formation = dump_yaml(stack)

    if not args.outfile: print(formation)
    else:
        with open(args.outfile, 'w') as f: f.write(formation)

# Execute if run as a script
if __name__ == "__main__":
    main()
