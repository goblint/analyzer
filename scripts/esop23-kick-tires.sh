#!/usr/bin/env bash
make test
ruby scripts/update_suite.rb group octagon -s
ruby scripts/update_suite.rb group apron -s
ruby scripts/update_suite.rb group apron2 -s
ruby scripts/update_suite.rb group apron-mukherjee -s
