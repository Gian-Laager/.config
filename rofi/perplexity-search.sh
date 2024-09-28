#!/bin/bash

# Function to URL encode a string
urlencode() {
    python3 -c "import urllib.parse, sys; print(urllib.parse.quote(sys.argv[1]))" "$1"
}

# Check if there's any input
if [ -n "$1" ]; then
    query="$@"
    encoded_query=$(urlencode "$query")
    chromium "https://www.perplexity.ai/search?q=$encoded_query" &
fi

return 0
