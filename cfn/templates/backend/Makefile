# Fetch CloudFlare IP addresses only once, when needed
CLOUDFLARE_IPS = $(eval CLOUDFLARE_IPS := $$(shell curl -X GET "https://api.cloudflare.com/client/v4/ips" | cut -d\[ -f 2 | cut -d\] -f1))$(CLOUDFLARE_IPS)

STACK_PARAMS		+= CloudFlareIPs=$(CLOUDFLARE_IPS)

include ../Makefile.included
