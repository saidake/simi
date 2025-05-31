package com.simi.common.util.network;

import jakarta.servlet.http.HttpServletRequest;
import lombok.experimental.UtilityClass;
import lombok.extern.slf4j.Slf4j;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@Slf4j
@UtilityClass
public class IpUtil {
    private final static String UNKNOWN_STR = "unknown";

    /**
     * Retrieves the client IP address of the user accessing the application, suitable for both public and local network environments.
     * This method checks multiple HTTP headers that may contain the client's real IP, including:
     * - X-Forwarded-For
     * - Proxy-Client-IP
     * - WL-Proxy-Client-IP
     * - HTTP_CLIENT_IP
     * - HTTP_X_FORWARDED_FOR
     * If none of these headers are available or valid, it falls back to the remote address from the request.
     * If the IP address is a loopback address (127.0.0.1 or equivalent), the server's internal IP is returned.
     */
    public static String getRemoteAddr(HttpServletRequest request) {
        String ip = request.getHeader("X-Forwarded-For");
        if (isEmptyIP(ip)) {
            ip = request.getHeader("Proxy-Client-IP");
            if (isEmptyIP(ip)) {
                ip = request.getHeader("WL-Proxy-Client-IP");
                if (isEmptyIP(ip)) {
                    ip = request.getHeader("HTTP_CLIENT_IP");
                    if (isEmptyIP(ip)) {
                        ip = request.getHeader("HTTP_X_FORWARDED_FOR");
                        if (isEmptyIP(ip)) {
                            ip = request.getRemoteAddr();
                            if ("127.0.0.1".equals(ip) || "0:0:0:0:0:0:0:1".equals(ip)) {
                                // If the IP is a loopback address, return the server's internal IP
                                ip = getServerInnerIP();
                            }
                        }
                    }
                }
            }
        } else if (ip.length() > 15) {
            String[] ips = ip.split(",");
            for (String strIp : ips) {
                if (!isEmptyIP(strIp)) {
                    ip = strIp;
                    break;
                }
            }
        }
        return ip;
    }

    /**
     * Checks whether an IP address string is considered empty or invalid.
     * An IP is considered empty if it is null, has zero length, or is the string "unknown" (case insensitive).
     */
    private static boolean isEmptyIP(String ip) {
        return ip == null || ip.length() == 0 || UNKNOWN_STR.equalsIgnoreCase(ip);
    }

    /**
     * Retrieves the server's internal IP address (typically used within a local network).
     * This method uses the InetAddress.getLocalHost() method to get the host address.
     * In case of failure, an empty string is returned.
     */
    public static String getServerInnerIP() {
        try {
            return InetAddress.getLocalHost().getHostAddress();
        } catch (UnknownHostException e) {
            log.error("Failed to retrieve the local host address", e);
        }
        return "";
    }

    /**
     * Retrieves the server's public IPv4 address by fetching data from a third-party service (http://ip.chinaz.com).
     * The method sends an HTTP request to this URL, retrieves the HTML content, and parses it to extract the public IP address.
     * The IP is extracted using a regular expression pattern that matches the appropriate HTML element containing the IP.
     */
    public static String getServerOutIP() {
        String ip = "";
        String chinaz = "http://ip.chinaz.com";
        StringBuilder inputLine = new StringBuilder();
        String read = "";
        URL url = null;
        HttpURLConnection urlConnection = null;
        BufferedReader in = null;
        try {
            url = new URL(chinaz);
            urlConnection = (HttpURLConnection) url.openConnection();
            in = new BufferedReader(new InputStreamReader(urlConnection.getInputStream(), "UTF-8"));
            while ((read = in.readLine()) != null) {
                inputLine.append(read).append("\r\n");
            }
        } catch (MalformedURLException e) {
            log.error("Malformed URL encountered while retrieving external IP", e);
        } catch (IOException e) {
            log.error("IOException occurred while retrieving external IP", e);
        } finally {
            if (in != null) {
                try {
                    in.close();
                } catch (IOException e) {
                    log.error("Error closing BufferedReader", e);
                }
            }
        }

        Pattern p = Pattern.compile("<dd class=\"fz24\">(.*?)</dd>");
        Matcher m = p.matcher(inputLine.toString());
        if (m.find()) {
            String ipstr = m.group(1);
            ip = ipstr;
        }
        return ip;
    }
}
