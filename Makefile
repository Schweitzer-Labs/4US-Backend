
SHELL                  := bash
export CREPES          := cfn/bin/crepes.py

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
       export SRCDIR	:= services/contribute/src
endif


export SAMDIR		:= .aws-sam
export BUILDDIR		:= $(SAMDIR)/build
export STACK		:= purple-4us

export STACKNAME	:= $(STACK)-$(RUNENV)

export DATE		:= $(shell date)
export NONCE		:= $(shell uuidgen | cut -d\- -f1)

export ENDPOINT		:= https://cloudformation-fips.$(REGION).amazonaws.com
export BUCKET		:= 4us-cfn-templates-$(REGION)

export STACK_PARAMS	+= LambdaRunEnvironment=$(RUNENV)

export TEMPLATE		:= template.yml
export PACKAGE		:= $(SAMDIR)/template.yaml

CFNDIR			:= cfn/templates
SRCS			:= $(shell find cfn/template/0* -name '*.yml' -o -name '*.txt')

.PHONY: dep build buildstacks check local package deploy clean realclean

# Make targets
dep:
	@pip3 install jinja2 cfn_tools

$(TEMPLATE): $(SRCS)
	@$(CREPES) --region $(REGION) --output $(TEMPLATE) $(CFNDIR)

build: $(TEMPLATE) $(SRCDIR)/app.js
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
