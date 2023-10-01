{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.mqtt2hue;
  pkg = cfg.package;
  homeDir = "/var/lib/mqtt2hue";
  certDir = "${homeDir}/certificate";
  settings = {
    netInterface = cfg.interface;
    usersFilePath = "${homeDir}/users.yaml";
    mqttBroker = cfg.brokerUrl;
    netmask = cfg.netmask;
    gateway = cfg.gateway;
    certificatePath = certDir;
    timezone = config.time.timeZone;
    httpPort = cfg.httpPort;
    httpsPort = cfg.httpsPort;
  };
  format = pkgs.formats.yaml { };
  configFile = format.generate "mqtt2hue.yaml" settings;
in {
  options.services.mqtt2hue = {
    package = mkOption {
      description = lib.mdDoc "package to use";
      # default = pkgs.mqtt2hue;
      # defaultText = literalExpression ''
      #   pkgs.mqtt2hue
      # '';
      type = types.package;
    };
    enable = mkOption {
      type = types.bool;
      default = false;
      description = "Whether to enable Mqtt2Hue.";
    };
    interface = mkOption {
      type = types.str;
      example = "wlp3s0";
      description = "The interface to listen to.";
    };
    brokerUrl = mkOption {
      type = types.str;
      example = "mqtt://<username>:<password>@192.168.1.123";
      description = "The URL of the MQTT broker.";
    };
    netmask = mkOption {
      type = types.str;
      example = "255.255.255.0";
      description = "Net mask";
    };
    gateway = mkOption {
      type = types.str;
      example = "255.255.255.0";
      description = "Gateway IP";
    };
    httpPort = mkOption {
      type = types.port;
      example = "80";
      description = "http port";
    };
    httpsPort = mkOption {
      type = types.port;
      example = "443";
      description = "https port";
    };
  };
  
  config = mkIf (cfg.enable) {
    users.users.mqtt2hue = {
      home = homeDir;
      createHome = true;
      group = "nginx";
      # uid = config.ids.uids.mqtt2hue;
      isSystemUser = true;
    };
    # users.groups.mqtt2hue.gid = config.ids.gids.mqtt2hue;
    systemd.services.mqtt2hue = {
      description = "mqtt2hue Service";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      serviceConfig = {
        ExecStart = "${cfg.package}/bin/mqtt2hue ${configFile}";
        User = "mqtt2hue";
        Group = "mqtt2hue";
        DynamicUser = true;
        StateDirectory = "mqtt2hue";
        WorkingDirectory = homeDir;
        Restart = "on-failure";
      };
    };
  };
}
