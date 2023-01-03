# app

To generate certificate:

```bash
openssl genrsa -out key.pem 2048
openssl req -new -key key.pem -out certificate.csr
openssl x509 -req -in certificate.csr -signkey key.pem -out certificate.pem
```

build: `stack build`

run server: `stack exec app-exe runServer`
