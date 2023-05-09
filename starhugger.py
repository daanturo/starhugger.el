#!/usr/bin/env python

import json
import sys

import requests

# API_URL = "https://api-inference.huggingface.co/models/gpt2"
API_URL = "https://api-inference.huggingface.co/models/bigcode/starcoder"

headers = {"Content-Type": "application/json"}

API_TOKEN = ""
if 0 < len(API_TOKEN):
    headers = {**headers, "Authorization": f"Bearer {API_TOKEN}"}


def query(payload):
    data = json.dumps({"inputs": payload})
    response = requests.request("POST", API_URL, headers=headers, data=data)
    return json.loads(response.content.decode("utf-8"))


data = query(sys.argv[0])

print(data)
