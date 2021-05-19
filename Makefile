
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

export SUBDOMAIN        := donate

ifeq ($(REGION), )
       export REGION	:= us-east-1
endif

export CONTRIB_DIR	:= services/policapital/contribute
export ONBOARD_DIR	:= services/policapital/onboard
export ANALYTICS_DIR	:= services/policapital/analytics
export RECORDER_DIR	:= services/policapital/receiver
export PLATFORM_DIR	:= services/platform
export EMAILER_DIR	:= services/emailer


export STACK		:= $(DOMAIN)-backend

export SAM_DIR		:= $(PWD)/.aws-sam
export BUILD_DIR	:= $(SAM_DIR)/build

export STACKNAME	:= $(STACK)-$(RUNENV)

export DATE		:= $(shell date)
export NONCE		:= $(shell uuidgen | cut -d\- -f1)

export ENDPOINT		:= https://cloudformation-fips.$(REGION).amazonaws.com
export BUCKET		:= 4us-cfn-templates-$(REGION)

export STACK_PARAMS	:= Nonce=$(NONCE)
STACK_PARAMS		+= LambdaRunEnvironment=$(RUNENV)

export PACKAGE		:= $(SAM_DIR)/CloudFormation-template.yml

CFN_DIR			:= cfn/templates

# stacks and templates
BACKEND_STACK		:= backend
BACKEND_TEMPLATE	:= $(BUILD_DIR)/$(BACKEND_STACK).yml
IAM_STACK		:= iam
IAM_TEMPLATE		:= $(BUILD_DIR)/$(IAM_STACK).yml
CONTRIBUTOR_RECEIPT	:= contributor-receipt
CONTRIBUTOR_TEMPLATE	:= $(BUILD_DIR)/$(CONTRIBUTOR_RECEIPT).yml
COMMITTEE_RECEIPT	:= committee-receipt
COMMITTEE_TEMPLATE	:= $(BUILD_DIR)/$(COMMITTEE_RECEIPT).yml

IMPORTS			:= $(BUILD_DIR)/Imports-$(STACK).yml

CONTRIB_APP		:= $(CONTRIB_DIR)/app.js
ONBOARD_APP		:= $(ONBOARD_DIR)/app.js
ANALYTICS_APP		:= $(ANALYTICS_DIR)/app.js
RECORDER_APP		:= $(RECORDER_DIR)/app.js
EMAILER_APP		:= $(EMAILER_DIR)/app.js

JS_APPS	:= $(CONTRIB_APP) $(ONBOARD_APP) $(ANALYTICS_APP) $(RECORDER_APP) $(EMAILER_APP)
CFN_TEMPLATES := $(BACKEND_TEMPLATE) $(IAM_TEMPLATE) $(CONTRIBUTOR_TEMPLATE) $(COMMITTEE_TEMPLATE)

.PHONY: dep build buildstacks check local import package deploy clean realclean

# Make targets
all: build
	$(MAKE) -C $(CFN_DIR)/$(BACKEND_STACK)

mkbuilddir:
	echo $(BUILD_DIR)
	mkdir -p $(BUILD_DIR)

build: clean mkbuilddir buildstacks buildsam

buildsam: buildstacks compile $(JS_APPS)
	sam build \
		--cached \
		--base-dir . \
		--template-file $(BACKEND_TEMPLATE)

buildstacks: mkbuilddir $(CFN_TEMPLATES)
	echo Built all the stacks

compile: $(CONTRIB_DIR)
	cd $^ && npm run compile && cd ../../../$(PLATFORM_DIR) && npm run compile


dep:
	@pip3 install jinja2 cfn_flip boto3

$(BUILD_DIR)/%.yml: $(CFN_DIR)/%
	$(MAKE) template=$@ -C $^ check


check: buildstacks
	$(MAKE) -c $(CFN_DIR)/$(BACKEND_STACK) check


clean:
	rm -f $(BUILD_DIR)/*.yml

realclean: clean
	@rm -rf $(SAM_DIR)

local: build
	@sam local start-api

package: build
	aws cloudformation package \
		--endpoint-url $(ENDPOINT) \
		--template-file $(BUILD_DIR)/template.yaml \
		--region $(REGION) \
		--s3-bucket $(BUCKET) \
		--s3-prefix $(STACKNAME) \
		--output-template-file $(PACKAGE)

deploy: package
	aws cloudformation deploy \
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
	@$(MAKE) -C $(CFN_DIR) import
