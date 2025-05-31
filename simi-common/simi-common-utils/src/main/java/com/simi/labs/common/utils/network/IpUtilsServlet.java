package com.simi.labs.common.utils.network;

import jakarta.servlet.http.HttpServletRequest;
import lombok.experimental.UtilityClass;
import lombok.extern.slf4j.Slf4j;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.*;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


/**
 * Utility class for extracting client IP addresses from servlet-based HTTP requests.
 * <p>
 * This class provides methods to accurately determine the originating IP address of a client
 * by inspecting common HTTP headers that may be set by proxies or load balancers (e.g., X-Forwarded-For).
 * Falls back to the remote address of the request if no headers are present.
 * </p>
 *
 * Designed for use in traditional Servlet environments (e.g., Spring MVC).
 *
 * @author Craig Brown
 * @since 1.2.1
 */
@Slf4j
@UtilityClass
public class IpUtilsServlet {
    private final static String UNKNOWN_STR = "unknown";

    // Common headers that may contain the client's real IP behind proxies/load balancers
    private final String[] POSSIBLE_CLIENT_IP_HEADERS = {
            "X-Forwarded-For",
            "Proxy-Client-IP",
            "WL-Proxy-Client-IP",
            "HTTP_CLIENT_IP",
            "HTTP_X_FORWARDED_FOR"
    };

    /**
     * Retrieves the client IP address of the user accessing the application,
     * suitable for both public and local network environments.
     * If none of these headers are available or valid, it falls back to the
     * remote address from the request. If the IP address is a loopback address
     * (127.0.0.1 or equivalent), the server's internal IP is returned.
     *
     * @param request Http request
     * @return IP Address
     */
    public static Optional<String> getRemoteAddr(HttpServletRequest request) {
        String ip = null;
        for (String header : POSSIBLE_CLIENT_IP_HEADERS) {
            ip = request.getHeader(header);
            if (!isEmptyIP(ip)) {
                break;
            }
        }
        if (isEmptyIP(ip)) {
            ip = request.getRemoteAddr();
            if ("127.0.0.1".equals(ip) || "0:0:0:0:0:0:0:1".equals(ip)) {
                ip = getServerInnerIP();
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
        return Optional.ofNullable(ip);
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
