
SHELL			:= bash
export CREPES		:= $(PWD)/cfn/bin/crepes.py

# Allowed values are: prod, qa, dev
ifeq ($(RUNENV), )
       export RUNENV	:= dev
endif

ifeq ($(RUNENV), prod)
	export DOMAIN	:= policapital
	export TLD	:= net
else
	export DOMAIN	:= purplepay
	export TLD	:= us
endif

ifeq ($(REGION), )
       export REGION	:= us-east-1
endif

ifeq ($(CONTRIB_DIR),)
       export CONTRIB_DIR	:= services/policapital/contribute
endif

ifeq ($(ONBOARD_DIR),)
       export ONBOARD_DIR	:= services/policapital/onboard
endif

ifeq ($(ANALYTICS_DIR),)
       export ANALYTICS_DIR	:= services/policapital/analytics
endif

ifeq ($(RECORDER_DIR),)
       export RECORDER_DIR	:= services/policapital/recorder
endif

ifeq ($(PLATFORM_DIR),)
       export PLATFORM_DIR	:= services/graphql-api
endif

ifeq ($(EMAILER_DIR),)
       export EMAILER_DIR	:= services/emailer
endif

export SUBDOMAIN        := donate

export STACK		:= $(DOMAIN)-backend

export SAMDIR		:= $(PWD)/.aws-sam
export BUILDDIR		:= $(SAMDIR)/build

export STACKNAME	:= $(STACK)-$(RUNENV)

export DATE		:= $(shell date)
export NONCE		:= $(shell uuidgen | cut -d\- -f1)

export ENDPOINT		:= https://cloudformation-fips.$(REGION).amazonaws.com
export BUCKET		:= 4us-cfn-templates-$(REGION)

export STACK_PARAMS	:= Nonce=$(NONCE)
STACK_PARAMS		+= LambdaRunEnvironment=$(RUNENV)

export TEMPLATE		:= template.yml
export PACKAGE		:= $(SAMDIR)/CloudFormation-template.yml

CFNDIR			:= $(PWD)/cfn/template
SRCS			:= $(shell find cfn/template/0* -name '*.yml' -o -name '*.txt')

IMPORTS			:= $(BUILDDIR)/Imports-$(STACK).yml

CONTRIB_APP		:= $(CONTRIB_DIR)/app.js
ONBOARD_APP		:= $(ONBOARD_DIR)/app.js
ANALYTICS_APP		:= $(ANALYTICS_DIR)/app.js
RECORDER_APP		:= $(RECORDER_DIR)/app.js
EMAILER_APP		:= $(EMAILER_DIR)/app.js

JS_APPS	:= $(CONTRIB_APP) $(ONBOARD_APP) $(ANALYTICS_APP) $(RECORDER_APP) $(EMAILER_APP)

.PHONY: dep build buildstacks check local import package deploy clean realclean

# Make targets
build: clean compile $(TEMPLATE) $(JS_APPS)
	@sam build --cached

compile: $(CONTRIB_DIR)
	cd $^ && npm run compile && cd ../../../$(PLATFORM_DIR) && npm run compile


dep:
	@pip3 install jinja2 cfn_flip boto3

$(TEMPLATE): $(SRCS)
	@$(CREPES) $(CFNDIR) \
		--region $(REGION) \
		--subdomain $(SUBDOMAIN) \
                --domain $(DOMAIN) \
                --tld $(TLD) \
		--runenv $(RUNENV) \
		--output $(TEMPLATE)


check: $(TEMPLATE)
	@echo "Validating template"
	@aws cloudformation validate-template --template-body file://$^


clean:
	@rm -f $(TEMPLATE) $(PACKAGE)

realclean: clean
	@rm -rf $(SAMDIR)

local: $(TEMPLATE)
	@sam local start-api

package: build check
	@aws cloudformation package \
		--endpoint-url $(ENDPOINT) \
		--template-file $(BUILDDIR)/template.yaml \
		--region $(REGION) \
		--s3-bucket $(BUCKET) \
		--s3-prefix $(STACKNAME) \
		--output-template-file $(PACKAGE)

deploy: package
	@aws cloudformation deploy \
		--endpoint-url $(ENDPOINT) \
		--region $(REGION) \
		--template-file $(PACKAGE) \
		--stack-name $(STACKNAME) \
		--s3-bucket $(BUCKET) \
		--s3-prefix $(STACKNAME) \
		--capabilities \
		       CAPABILITY_NAMED_IAM \
		       CAPABILITY_AUTO_EXPAND \
		--parameter-overrides \
			$(STACK_PARAMS)

	
import: build
	@$(MAKE) -C $(CFNDIR) import
