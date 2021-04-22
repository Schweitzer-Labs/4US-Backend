IMPORTS			:= $(BUILDDIR)/Imports-$(STACK).yml

.PHONY: dep build buildstacks check local package deploy clean realclean

buildimports: $(IMPORTS)

$(IMPORTS): ImportedResources/$(REGION).yml $(SRCS)
	@echo "Importing resources from $(REGION) into $(STACK) via $@"
	@$(CREPES) . --region $(REGION) --import $@ --output $(TEMPLATE)

import: $(IMPORTS)
	@echo "Creating import changeset for CloudFormation template"
	@aws cloudformation create-change-set \
		--endpoint-url $(ENDPOINT) \
		--stack-name $(STACKNAME) \
		--change-set-name ImportChangeSet \
		--change-set-type IMPORT \
		--resources-to-import file://$(IMPORTS) \
		--template-body file://$(TEMPLATE) \
		--capabilities \
			CAPABILITY_NAMED_IAM \
		--region $(REGION)
	@aws cloudformation describe-change-set \
		--endpoint-url $(ENDPOINT) \
		--stack-name=$(STACKNAME) \
		--change-set-name ImportChangeSet \
		--region=$(REGION)
	@echo "Waiting for the changeset to finish creating"
	@aws cloudformation wait change-set-create-complete \
		--endpoint-url $(ENDPOINT) \
		--stack-name=$(STACKNAME) \
		--change-set-name ImportChangeSet \
		--region=$(REGION)
	@echo "Executing the changeset"
	@aws cloudformation execute-change-set \
		--endpoint-url $(ENDPOINT) \
		--stack-name=$(STACKNAME) \
		--change-set-name ImportChangeSet \
		--region=$(REGION)
	@aws cloudformation wait stack-import-complete \
		--endpoint-url $(ENDPOINT) \
		--stack-name=$(STACKNAME) \
		--region=$(REGION)
