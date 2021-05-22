
SHELL			:= bash
export CREPES		:= $(PWD)/cfn/bin/crepes.py

# Allowed values are: prod, qa, demo
ifeq ($(RUNENV),)
	export RUNENV	:= qa
endif

ifeq ($(PRODUCT), )
        export PRODUCT	:= p2
endif

ifeq ($(RUNENV), qa)
        export REGION   := us-west-2
	ifeq ($(PRODUCT), 4us)
		export DOMAIN   := 4us
		export TLD      := net
	else # PRODUCT = p2
		export DOMAIN   := purplepay
		export TLD      := us
	endif
else ifeq ($(RUNENV), prod)
        export REGION   := us-east-1
	ifeq ($(PRODUCT), 4us)
		export DOMAIN   := 4us
		export TLD      := net
	else
		export DOMAIN   := policapital
		export TLD      := net
	endif
else ifeq ($(RUNENV), backup)
        export REGION   := us-west-1
	ifeq ($(PRODUCT), p2)
		export DOMAIN   := 4us
		export TLD      := net
	else
		export DOMAIN   := policapital
		export TLD      := net
	endif
else # demo
        export REGION   := us-east-2
        export DOMAIN   := 4usdemo
        export TLD      := com
endif

export CONTRIB_DIR	:= lambdas
export ONBOARD_DIR	:= lambdas
export ANALYTICS_DIR	:= lambdas
export RECORDER_DIR	:= lambdas
export PLATFORM_DIR	:= lambdas
export EMAILER_DIR	:= lambdas


export STACK		:= $(RUNENV)-$(PRODUCT)-backend

export SAM_DIR		:= $(PWD)/.aws-sam
export BUILD_DIR	:= $(PWD)/build

export DATE		:= $(shell date)
export NONCE		:= $(shell uuidgen | cut -d\- -f1)

export ENDPOINT		:= https://cloudformation-fips.$(REGION).amazonaws.com
export CFN_BUCKET	:= $(PRODUCT)-cfn-templates-$(REGION)

export STACK_PARAMS	:= Nonce=$(NONCE)
STACK_PARAMS		+= LambdaRunEnvironment=$(RUNENV) Product=$(PRODUCT)
STACK_PARAMS		+= Domain=$(DOMAIN) TLD=$(TLD)
ifneq ($(SUBDOMAIN),)
	STACK_PARAMS	+= SubDomain=$(SUBDOMAIN)
endif

export PACKAGE		:= $(SAM_DIR)/CloudFormation-template.yml

CFN_DIR			:= cfn/templates

# stacks and templates
BACKEND_STACK		:= backend
BACKEND_TEMPLATE	:= $(BUILD_DIR)/$(BACKEND_STACK).yml
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
CFN_TEMPLATES := $(BACKEND_TEMPLATE) $(CONTRIBUTOR_TEMPLATE) $(COMMITTEE_TEMPLATE)

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

compile:
	cd services && npm run compile


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
		--template-file $(SAM_DIR)/build/template.yaml \
		--region $(REGION) \
		--s3-bucket $(CFN_BUCKET) \
		--s3-prefix $(STACK) \
		--output-template-file $(PACKAGE)

deploy: package
	aws cloudformation deploy \
		--endpoint-url $(ENDPOINT) \
		--region $(REGION) \
		--template-file $(PACKAGE) \
		--stack-name $(STACK) \
		--s3-bucket $(CFN_BUCKET) \
		--s3-prefix $(STACK) \
		--capabilities \
		       CAPABILITY_NAMED_IAM \
		       CAPABILITY_AUTO_EXPAND \
		--parameter-overrides \
			$(STACK_PARAMS)

	
import: build
	@$(MAKE) -C $(CFN_DIR) import
