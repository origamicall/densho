densho
======

[![License: LGPL 2.1](https://img.shields.io/badge/License-GNU%20Lesser%20General%20Public%20License%20v2.1-blue.svg)](https://raw.githubusercontent.com/origamicall/densho/master/COPYING)

Densho (in Japanesse 伝書 , couriers) is designed to connect to different SMS providers and give you a route to send a SMS.

**IMPORTANT** This project is still under strong development.

## Configuration

The configuration of densho is as follow (for the `app.config` or `sys.config` files):

```erlang
{densho,[
    {sms_providers_order, [lleida]},
    {sms_providers_config, [
        {lleida, [
            {url, "api.lleida.net"},
            {uri, "sms/v1"},
            {username, "username"},
            {password, "password"}]
        }
    ]}
]},
```

## Backends

### Lleida

This is a Spanish company that gives professional solutions for SMS sending. The configuration is as above (in the previous example).

You can configure the following parameters:

- `url`: the URL for Lleida API web site.
- `uri`: the URI for Lleida API.
- `username`: the user for your Lleida account.
- `password`: the password for your Lleida account.

