#!/usr/bin/env python3


import base64
import quopri
import json
import sys

from faker import Faker


def main():
    fake = Faker()

    message = json.load(sys.stdin)

    text = fake.text(max_nb_chars=75)
    encoded = base64.b64encode(text.encode('utf8')).decode('utf8')

    message["snippet"] = text
    message["payload"]["body"] = {
        'size': len(encoded),
        'data': encoded,
        }
    dumped = json.dumps(message, indent=1)

    print('length = {}'.format(len(dumped)))
    sys.stdout.write(dumped)


if __name__ == '__main__':
    main()
