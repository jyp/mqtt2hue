
mac=90:61:ae:21:8f:6d
serial="${mac:0:2}${mac:3:2}${mac:6:2}fffe${mac:9:2}${mac:12:2}${mac:15:2}"
openssl req -new -config openssl.conf  -nodes -x509 -newkey  ec -pkeyopt ec_paramgen_curve:P-256 -pkeyopt ec_param_enc:named_curve   -subj "/C=NL/O=Philips Hue/CN=$serial" -out cert.pem -set_serial "0x$serial" -days 3650
