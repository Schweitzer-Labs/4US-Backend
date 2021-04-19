
SHELL                  := bash
export CREPES          := $(PWD)/bin/crepes.py

ifeq ($(REGION), )
       export REGION   := us-east-1
endif

ifeq ($(REGION), us-east-1)
       export ENVNAME  := production
else ifeq ($(REGION), us-west-2)
       export ENVNAME  := staging
else ifeq ($(REGION), us-west-1)
       export ENVNAME  := devops
else ifeq ($(REGION), us-east-2)
       export ENVNAME  := backup
endif

ifeq ($(SRCDIR),)
       export SRCDIR   := src
endif


export BUILDDIR                := .aws-sam/build
export STACK           := purple-4us

export STACKNAME       := $(STACK)-$(ENVNAME)

export DATE            := $(shell date)
export NONCE           := $(shell uuidgen | cut -d\- -f1)

export ENDPOINT        := https://cloudformation-fips.$(REGION).amazonaws.com
export BUCKET          := 4us-cfn-templates-$(REGION)

export STACK_PARAMS    += Nonce=$(NONCE)

export TEMPLATE                := $(BUILDDIR)/template.yaml


.PHONY: build buildstacks check local package deploy clean realclean

# Make targets

build: $(SRCDIR)/contribute/app.js
	@sam build

check: build
	@echo npm something something

clean:
	@rm $(TEMPLATE)

realclean: clean
	@rm -rf $(BUILDDIR)

local: $(TEMPLATE)
	@sam local invoke

package: check
	@aws cloudformation package \
		--endpoint-url $(ENDPOINT) \
		--template-file $(TEMPLATE) \
		--region $(REGION) \
		--s3-bucket $(BUCKET) \
		--s3-prefix $(STACKNAME) \
		--output-template-file $(PACKAGE)

deploy: check
	@echo "Actual command to deploy the packaged template:"
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
