#!/bin/sh
set -ue

external_ip=`dig +short txt ch whoami.cloudflare @1.0.0.1 | tr -d '"'`
current_config=`dig +short $CF_DOMAIN`

if [ "$external_ip" = "$current_config" ]; then
	# no need to work
	exit 0
fi

cf_response=$(curl -s -X PUT "https://api.cloudflare.com/client/v4/zones/$CF_ZONE_ID/dns_records/$CF_RECORD_ID" \
     -H "Authorization: Bearer $CF_API_TOKEN" \
     -H "Content-Type: application/json" \
     --data '{"type":"A","name":"'"$CF_DOMAIN"'","content":"'"$external_ip"'","ttl":60,"proxied":false}')

if ! echo "$cf_response" | grep -q '"success":true'; then
    echo "Failed to update DNS record."
    exit 1
fi
