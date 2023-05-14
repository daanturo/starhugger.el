#!/usr/bin/env python

# https://huggingface.co/docs/api-inference/quicktour

import json
import os
import random
import sys
from pprint import pprint

import requests

# API_URL = "https://api-inference.huggingface.co/models/gpt2"
API_URL = "https://api-inference.huggingface.co/models/bigcode/starcoder"

headers = {"Content-Type": "application/json"}

API_TOKEN = os.getenv("TOKEN", "")
if 0 < len(API_TOKEN):
    headers = {**headers, "Authorization": f"Bearer {API_TOKEN}"}

params = {
    "return_full_text": False,
    "num_return_sequences": 2,
    # "max_new_tokens": 256,
    # "temperature": random.uniform(0, 2.0)
}

if os.getenv("TEMPERATURE"):
    params = {**params, "temperature": float(os.getenv("TEMPERATURE"))}


def query(payload):
    data = json.dumps(
        {
            "inputs": payload,
            "parameters": params,
            "options": {
                "use_cache": False,
                #
            },
        }
    )
    pprint(data)
    response = requests.request("POST", API_URL, headers=headers, data=data)
    parsed = json.loads(response.content.decode("utf-8"))
    return parsed


# <fim_prefix>〈code before〉<fim_middle>〈code after〉<fim_suffix>

pprint(query(" ".join(sys.argv[1:])))
