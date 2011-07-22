function FindProxyForURL(url, host)
{
        if (isPlainHostName(host) ||
            isInNet(host, "127.0.0.0",   "255.255.255.0") ||
            isInNet(host, "192.168.0.0", "255.255.0.0") ||
            isInNet(host, "10.0.0.0",    "255.0.0.0"))
                return "DIRECT";

        if (dnsDomainIs(host, ".logica.com") &&
            host != "mail.logica.com" &&
            !dnsDomainIs(host, ".define.logica.com"))
                return "DIRECT";

        return "SOCKS localhost:9999";
}
