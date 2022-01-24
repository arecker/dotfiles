#!/usr/bin/env bash
set -e

cd ~/src/blog

python -m src jenkins \
       --netlify-token "$(pass netlify/jenkins)" \
       --slack-webhook-urls "$(pass slack/reckers/webhook)" \
       --twitter-consumer-api-key "$(pass twitter/reckerbot/consumer-api-key)" \
       --twitter-consumer-api-secret-key "$(pass twitter/reckerbot/consumer-api-secret-key)" \
       --twitter-access-token "$(pass twitter/reckerbot/access-token)" \
       --twitter-access-token-secret "$(pass twitter/reckerbot/access-token-secret)"
