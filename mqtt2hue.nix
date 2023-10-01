{ mkDerivation, aeson, attoparsec, base, base-compat, blaze-html
, blaze-markup, bytestring, Cabal, containers, directory, fetchFromGitHub
, hashable, http-api-data, http-media, http-types, large-hashable
, lib, lucid, mtl, net-mqtt, network, network-info
, network-multicast, network-uri, servant, servant-server
, servant-xml, string-conversions, template-haskell, text, time
, unordered-containers, vector, wai, wai-enforce-https, wai-extra
, wai-logger, warp, warp-tls, xmlbf, yaml
}:
mkDerivation {
  pname = "mqtt2hue";
  version = "1.0";
  src = fetchFromGitHub {
    owner = "jyp";
    repo = "mqtt2hue";
    # sha256 = "0fi3pq1bf89bk5iixphvhi6v84vnn6s22z90k14ac9qjvwy6f2dx";
    hash = "sha256-Fl4slKplgSfPndDJxOcBxBCtibNCi6gBm/wNSIM6xgE=";
    rev = "de52519f5f7efb87dbce2c0a69c6858997de5204";
  };
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson attoparsec base base-compat blaze-html blaze-markup
    bytestring Cabal containers directory hashable http-api-data
    http-media http-types large-hashable lucid mtl net-mqtt network
    network-info network-multicast network-uri servant servant-server
    servant-xml string-conversions template-haskell text time
    unordered-containers vector wai wai-enforce-https wai-extra
    wai-logger warp warp-tls xmlbf yaml
  ];
  description = "Hue bridge emulator for Zigbee2MQTT system";
  license = "AGPL3";
  mainProgram = "mqtt2hue";
}
