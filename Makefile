SHELL				:= bash
export CREPES			:= $(PWD)/cfn/bin/crepes.py
export PRODUCT			:= 4us
export SAM_CLI_TELEMETRY	:=0

# Allowed values are: prod, qa, demo
ifeq ($(RUNENV),)
	export RUNENV	:= qa
endif

ifeq ($(RUNENV), qa)
        export REGION   := us-west-2
	export DOMAIN   := build4
	export TLD      := us
else ifeq ($(RUNENV), prod)
        export REGION   := us-east-1
		export DOMAIN   := 4us
		export TLD      := net
else ifeq ($(RUNENV), demo)
        export REGION   := us-west-1
	export DOMAIN   := 4usdemo
	export TLD      := com
endif

export STACK		:= $(RUNENV)-$(PRODUCT)-backend

export BUILD_DIR	:= $(PWD)/.build
export SAM_DIR		:= $(BUILD_DIR)/sam
export CFN_BUILD_DIR	:= $(BUILD_DIR)/cloudformation
export SAM_BUILD_DIR	:= $(SAM_DIR)/build
export SAM_CACHE_DIR	:= $(SAM_DIR)/cache

export DATE		:= $(shell date)
export NONCE		:= $(shell uuidgen | cut -d\- -f1)

export ENDPOINT		:= https://cloudformation-fips.$(REGION).amazonaws.com
export CFN_BUCKET	:= $(PRODUCT)-cfn-templates-$(REGION)

export STACK_PARAMS	:= Nonce=$(NONCE)
STACK_PARAMS		+= LambdaRunEnvironment=$(RUNENV)
STACK_PARAMS		+= Domain=$(DOMAIN) TLD=$(TLD)

ifneq ($(SUBDOMAIN),)
	STACK_PARAMS	+= SubDomain=$(SUBDOMAIN)
endif

export PACKAGE		:= $(CFN_BUILD_DIR)/CloudFormation-template.yml

CFN_SRC_DIR		:= cfn/templates

# Nested stacks and templates
BACKEND_STACK		:= backend
BACKEND_TEMPLATE	:= $(CFN_BUILD_DIR)/$(BACKEND_STACK).yml

CONTRIBUTOR_RECEIPT	:= contributor-receipt
CONTRIBUTOR_TEMPLATE	:= $(CFN_BUILD_DIR)/$(CONTRIBUTOR_RECEIPT).yml

COMMITTEE_RECEIPT	:= committee-receipt
COMMITTEE_TEMPLATE	:= $(CFN_BUILD_DIR)/$(COMMITTEE_RECEIPT).yml

DYNAMO_DBS		:= dynamodbs
DYNAMODB_TEMPLATE	:= $(CFN_BUILD_DIR)/$(DYNAMO_DBS).yml

CLOUDFLARE_BUILDER	:= cloudflare-builder
CLOUDFLARE_TEMPLATE	:= $(CFN_BUILD_DIR)/$(CLOUDFLARE_BUILDER).yml


IMPORTS			:= $(CFN_BUILD_DIR)/Imports-$(STACK).yml

export CONTRIB_DIR	:= lambdas
CONTRIB_APP		:= $(CONTRIB_DIR)/app.js

export ONBOARD_DIR	:= lambdas
ONBOARD_APP		:= $(ONBOARD_DIR)/app.js

export ANALYTICS_DIR	:= lambdas
ANALYTICS_APP		:= $(ANALYTICS_DIR)/app.js

export RECORDER_DIR	:= lambdas
RECORDER_APP		:= $(RECORDER_DIR)/app.js

export EMAILER_DIR	:= lambdas
EMAILER_APP		:= $(EMAILER_DIR)/app.js

export PLATFORM_DIR	:= lambdas


JS_APPS	:= $(CONTRIB_APP) $(ONBOARD_APP) $(ANALYTICS_APP) $(RECORDER_APP) $(EMAILER_APP)

EMAIL_TEMPLATES		:= $(CONTRIBUTOR_TEMPLATE) $(COMMITTEE_TEMPLATE)
export SAM_TEMPLATE	:= $(SAM_BUILD_DIR)/template.yaml

ifeq ($(REGION), us-west-1)
	CFN_TEMPLATES := $(BACKEND_TEMPLATE) $(DYNAMODB_TEMPLATE) $(CLOUDFLARE_TEMPLATE)
else ifeq ($(REGION), us-east-2)
	CFN_TEMPLATES := $(BACKEND_TEMPLATE)
else
	CFN_TEMPLATES := $(BACKEND_TEMPLATE) $(DYNAMODB_TEMPLATE) $(EMAIL_TEMPLATES) $(CLOUDFLARE_TEMPLATE)
endif

.PHONY: dep build build-stacks check local import package deploy clean realclean

# Make targets
all: build
	@$(MAKE) -C $(CFN_SRC_DIR)/$(BACKEND_STACK)

build: mkbuilddir build-stacks build-sam

mkbuilddir:
	@mkdir -p $(SAM_BUILD_DIR) $(CFN_BUILD_DIR)

build-sam: build-stacks $(SAM_TEMPLATE)

$(SAM_TEMPLATE): $(JS_APPS)
	@sam build \
		--cached \
		--parallel \
		--base-dir $(PWD) \
		--build-dir $(SAM_BUILD_DIR) \
		--cache-dir $(SAM_CACHE_DIR) \
		--template-file $(BACKEND_TEMPLATE)

build-stacks: mkbuilddir $(CFN_TEMPLATES)

compile:
	@npm -C services run compile

dep:
	@pip3 install jinja2 cfn_flip boto3

$(CFN_BUILD_DIR)/%.yml: $(CFN_SRC_DIR)/%
	$(MAKE) template=$@ -C $^ check

clean:
	@rm -f $(CFN_BUILD_DIR)/*.yml

realclean: clean
	@rm -rf $(BUILD_DIR)

local: build
	@sam local start-api \
		--warm-containers EAGER \
		--template-file $(SAM_BUILD_DIR)/template.yaml

check: build-stacks
	@$(MAKE) -C $(CFN_SRC_DIR)/$(BACKEND_STACK) $@

import: mkbuilddir
	@$(MAKE) -C $(CFN_SRC_DIR)/$(BACKEND_STACK) $@

package: build
	@$(MAKE) -C $(CFN_SRC_DIR)/$(BACKEND_STACK) $@

deploy: package
	@$(MAKE) -C $(CFN_SRC_DIR)/$(BACKEND_STACK) $@
