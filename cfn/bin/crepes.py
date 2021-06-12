#!/usr/bin/env python3

# Copyright 2020-2021 Seemant Kulleen <seemantk@gmail.com>

import os, boto3
import argparse
from cfn_tools import load_yaml, dump_yaml, dump_json
from jinja2 import Template

#
# Available command line arguments and their defaults
#
parser = argparse.ArgumentParser(description='process jinja cloudformation yaml files and assemble into a CloudFormation template')
parser.add_argument('directory', metavar='dir', type=str, help='src directory')
parser.add_argument('--region', dest='region', type=str, default='us-west-1', help='AWS Region')
parser.add_argument('--output', dest='outfile', type=str, help='output CloudFormation YAML file')
parser.add_argument('--import', dest='imports', type=str, help='import a subset of existing resources into the stack')
parser.add_argument('--runenv', dest='runenv', type=str, help='lambda run environment (prod/qa/demo)')
parser.add_argument('--domain', dest='domain', type=str, help="domain name (e.g. 'example' from 'www.example.com')")
parser.add_argument('--subdomain', dest='subdomain', type=str, help="subdomain (e.g. 'www' from 'www.example.com')")
parser.add_argument('--tld', dest='tld', type=str, help="tld (e.g. 'com' from 'www.example.com')")
parser.add_argument('--product', dest='product', type=str, help="the product (p2 or 4us) for which we are building this template")
parser.add_argument('--hostid', dest='hostid', type=str, help="route53 hostid")


#
# Set Global Variables
#
# Parse command line arguments and process the information
args = parser.parse_args()
src = os.path.dirname(args.directory)
stack = os.path.basename(args.directory)

runenv = args.runenv or 'dev'
subdomain = args.subdomain or ''
domain = args.domain or ''
tld = args.tld or ''

# Find AWS metadata for this stack deployment
ec2 = boto3.setup_default_session(region_name=args.region)
ec2 = boto3.client('ec2')
zones = ec2.describe_availability_zones()['AvailabilityZones']
AZs = [z['ZoneName'] for z in zones]
AZcodes = [z.split('-')[2] for z in AZs]


imports = {}

#
# Helper functions
#
# Read raw yaml file and process as a Jinja template
#
def process_jinja_template(filename):
    with open(filename) as f:
        contents = f.read()

    template = Template(contents)
    return template.render(AZs=AZs, AZcodes=AZcodes, REGION=args.region, RUNENV=runenv, SUBDOMAIN=subdomain, DOMAIN=domain, TLD=tld, PRODUCT=args.product, HOSTID=args.hostid)

# Assemble all the components of a stack into a single cloudformation::stack object
def assemble(stack):
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
                contents = load_yaml(process_jinja_template(os.path.join(dirpath, f)))

                try: # after jinja, we have pure yaml
                    if sec == 'Resources' and args.imports:
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
    for prop in imports[resource]['Property']:
        print('Importing %s for %s' % (prop, resource))
        try:
            temp[prop] = obj['Properties'][prop]
        except KeyError: # e.g. SNSTopic requires TopicARN for import, but it is not a property in the template
            pass

    # Replace the Properties key with only the filtered items
    obj['Properties'] = temp

    return { 'ResourceType': obj['Type'], 'LogicalResourceId': resource, 'ResourceIdentifier': imports[resource]['Matcher'] }


# Main loop
def main():
    global imports

    if args.imports: # process the ImportedResources file
        imports = load_yaml(process_jinja_template('ImportedResources.yml'))

    # Assemble the stack into a dict and convert that to yaml
    stack = assemble(args.directory)


    if args.imports: # trim the stack based on the ImportedResources
        stack.pop('Outputs', None)
        imports_list = [importify(res, stack['Resources'][res]) for res in stack['Resources']]

        print(dump_json(imports_list))
        with open(args.imports, 'w') as f: f.write(dump_json(imports_list))


    formation = dump_yaml(stack)

    if not args.outfile: print(formation)
    else:
        with open(args.outfile, 'w') as f: f.write(formation)

# Execute if run as a script
if __name__ == "__main__":
    main()
