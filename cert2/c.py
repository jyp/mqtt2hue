from cryptography import x509
from cryptography.hazmat.backends import default_backend
from cryptography.hazmat.primitives import hashes, serialization
from cryptography.hazmat.primitives.asymmetric import ec
from cryptography.x509 import NameOID
from datetime import *

def check_certificate(cert_file: str):
    """Check existing certificate file if the bridge id (mac address) matches."""
    if not os.path.isfile(cert_file):
        return False
    with open(cert_file) as fileobj:
        cert_pem = fileobj.read().encode("utf-8")
    cert = x509.load_pem_x509_certificate(cert_pem, default_backend())
    try:
        names = cert.subject.get_attributes_for_oid(NameOID.COMMON_NAME)
        cert_cn = names[0].value
        # return if certificate CN matched the bridge id
        return cert_cn == config.bridge_id.lower()
    except x509.ExtensionNotFound:
        return False

def generate_selfsigned_cert(bridge_id:str, cert_file: str, key_file: str) -> None:
    """Generate self signed certificate compatible with Philips HUE."""

    dec_serial = int(bridge_id.lower(), 16)

    root_key = ec.generate_private_key(ec.SECP256R1(), default_backend())
    subject = issuer = x509.Name(
        [
            x509.NameAttribute(NameOID.COUNTRY_NAME, "NL"),
            x509.NameAttribute(NameOID.ORGANIZATION_NAME, "Philips Hue"),
            x509.NameAttribute(NameOID.COMMON_NAME, bridge_id.lower()),
        ]
    )
    root_cert = (
        x509.CertificateBuilder()
        .subject_name(subject)
        .issuer_name(issuer)
        .public_key(root_key.public_key())
        .serial_number(dec_serial)
        .not_valid_before(datetime.utcnow())
        .not_valid_after(datetime.utcnow() + timedelta(days=3650))
        .add_extension(x509.BasicConstraints(ca=False, path_length=None), critical=True)
        .add_extension(
            x509.SubjectKeyIdentifier(b"hash").from_public_key(root_key.public_key()),
            critical=False,
        )
        .add_extension(
            x509.AuthorityKeyIdentifier(
                key_identifier=b"keyid,issuer",
                authority_cert_issuer=None,
                authority_cert_serial_number=None,
            ).from_issuer_public_key(root_key.public_key()),
            critical=False,
        )
        .add_extension(
            x509.KeyUsage(
                digital_signature=True,
                content_commitment=False,
                key_encipherment=True,
                data_encipherment=False,
                key_agreement=False,
                key_cert_sign=False,
                crl_sign=False,
                encipher_only=False,
                decipher_only=False,
            ),
            critical=True,
        )
        .add_extension(x509.ExtendedKeyUsage([x509.OID_SERVER_AUTH]), critical=False)
        .sign(root_key, hashes.SHA256(), default_backend())
    )

    cert_pem = root_cert.public_bytes(encoding=serialization.Encoding.PEM)

    key_pem = root_key.private_bytes(
        encoding=serialization.Encoding.PEM,
        format=serialization.PrivateFormat.TraditionalOpenSSL,
        encryption_algorithm=serialization.NoEncryption(),
    )

    with open(cert_file, "w") as fileobj:
        fileobj.write(cert_pem.decode("utf-8"))
    with open(key_file, "w") as fileobj:
        fileobj.write(key_pem.decode("utf-8"))
    print("Certificate generated")

generate_selfsigned_cert("9061aefffe218f6d","cert.pem","privkey.pem")
