
SHELL                  := bash
export CREPES          := $(PWD)/bin/crepes.py

ifeq ($(RUNENV), )
       export RUNENV	:= prod
endif

ifeq ($(RUNENV), prod)
       export REGION:= us-east-1
else ifeq ($(RUNENV), qa)
       export REGION  := us-west-2
else ifeq ($(RUNENV), dev)
       export REGION  := us-west-1
else ifeq ($(RUNENV), backup)
       export REGION  := us-east-2
endif

ifeq ($(SRCDIR),)
       export SRCDIR   := services/contribute/src
endif


export SAMDIR          := .aws-sam
export BUILDDIR        := $(SAMDIR)/build
export STACK           := purple-4us

export STACKNAME       := $(STACK)-$(RUNENV)

export DATE            := $(shell date)
export NONCE           := $(shell uuidgen | cut -d\- -f1)

export ENDPOINT        := https://cloudformation-fips.$(REGION).amazonaws.com
export BUCKET          := 4us-cfn-templates-$(REGION)

export STACK_PARAMS    += LambdaRunEnvironment=$(RUNENV)

export TEMPLATE        := $(BUILDDIR)/template.yaml


.PHONY: build buildstacks check local package deploy clean realclean

# Make targets

build: $(SRCDIR)/app.js
	@sam build

check: build
	@echo npm something something

clean:
	@rm -f $(TEMPLATE)

realclean: clean
	@rm -rf $(SAMDIR)

local: $(TEMPLATE)
	@sam local start-api

package: check
	@aws cloudformation package \
		--endpoint-url $(ENDPOINT) \
		--template-file $(TEMPLATE) \
		--region $(REGION) \
		--s3-bucket $(BUCKET) \
		--s3-prefix $(STACKNAME) \
		--output-template-file $(PACKAGE)

deploy: check
	sam deploy \
		--region $(REGION) \
		--template-file $(TEMPLATE) \
		--stack-name $(STACKNAME) \
		--s3-bucket $(BUCKET) \
		--s3-prefix $(STACKNAME) \
		--capabilities \
		       CAPABILITY_NAMED_IAM \
		       CAPABILITY_AUTO_EXPAND \
		--parameter-overrides \
			$(STACK_PARAMS) \
		--no-confirm-changeset \
		--no-fail-on-empty-changeset
