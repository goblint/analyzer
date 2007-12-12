#!/bin/bash
find -exec chgrp goblin {} \;
find -type d -exec chmod 2775 {} \;
find -type f -exec chmod 0664 {} \;
find -name "*.sh" -exec chmod 0775 {} \;
find -name "*.rb" -exec chmod 0775 {} \;
