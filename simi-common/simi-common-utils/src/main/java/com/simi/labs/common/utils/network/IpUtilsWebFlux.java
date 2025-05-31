package com.simi.labs.common.utils.network;

import lombok.experimental.UtilityClass;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Schedulers;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.*;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Utility class for extracting client IP addresses in Spring WebFlux applications.
 * Provides reactive methods to retrieve IPs from request headers (e.g., X-Forwarded-For)
 * or the server's remote address, with fallback to the server's internal IP for loopback addresses.
 *
 * @author Craig Brown
 * @since 1.2.1
 */
@Slf4j
@UtilityClass
public class IpUtilsWebFlux {
    private final static String UNKNOWN_IP = "unknown";

    // Common headers that may contain the client's real IP behind proxies/load balancers
    private static final List<String> CLIENT_IP_HEADERS = List.of(
            "X-Forwarded-For",
            "Proxy-Client-IP",
            "WL-Proxy-Client-IP",
            "HTTP_CLIENT_IP",
            "HTTP_X_FORWARDED_FOR"
    );

    /**
     * Retrieves the client IP address from a ServerWebExchange.
     * Checks client IP headers first, then falls back to the remote address or server internal IP.
     *
     * @param exchange The ServerWebExchange containing the request.
     * @return A Mono emitting the client IP address or "unknown" if none is found.
     */
    public static Mono<String> getRemoteAddr(ServerWebExchange exchange) {
        return Mono.defer(() -> {
            String ipFromHeaders = CLIENT_IP_HEADERS.stream()
                    .map(header -> {
                        String value = exchange.getRequest().getHeaders().getFirst(header);
                        log.debug("Header {}: {}", header, value);
                        return value;
                    })
                    .filter(IpUtilsWebFlux::isValidIP) // Explicit null check
                    .findFirst()
                    .orElse(null);

            if (isValidIP(ipFromHeaders)) {
                String clientIp = Arrays.stream(ipFromHeaders.split(","))
                        .map(String::trim)
                        .filter(IpUtilsWebFlux::isValidIP)
                        .findFirst()
                        .orElse(ipFromHeaders);
                log.debug("Selected client IP from headers: {}", clientIp);
                return Mono.just(clientIp);
            }

            String remoteIp = exchange.getRequest().getRemoteAddress() != null
                    ? exchange.getRequest().getRemoteAddress().getAddress().getHostAddress()
                    : null;
            log.debug("Remote IP: {}", remoteIp);

            if (remoteIp != null && isLoopbackAddress(remoteIp)) {
                return getServerInnerIP()
                        .switchIfEmpty(Mono.just(UNKNOWN_IP))
                        .doOnNext(ip -> log.debug("Loopback detected, using inner IP: {}", ip));
            }

            return Mono.justOrEmpty(remoteIp)
                    .switchIfEmpty(Mono.just(UNKNOWN_IP))
                    .doOnNext(ip -> log.debug("Using remote IP or fallback: {}", ip));
        });
    }

    /**
     * Checks if an IP address string is valid and not empty.
     * An IP is invalid if it is null, empty, or equals "unknown" (case-insensitive).
     *
     * @param ip The IP address string to check.
     * @return True if the IP is valid, false otherwise.
     */
    private static boolean isValidIP(String ip) {
        return !(ip == null || ip.isEmpty() || UNKNOWN_IP.equalsIgnoreCase(ip));
    }

    /**
     * Checks if an IP address is a loopback address (e.g., 127.0.0.1, ::1).
     *
     * @param ip The IP address to check.
     * @return True if the IP is a loopback address, false otherwise.
     */
    private static boolean isLoopbackAddress(String ip) {
        try {
            return InetAddress.getByName(ip).isLoopbackAddress();
        } catch (UnknownHostException e) {
            log.warn("Invalid IP address: {}", ip, e);
            return false;
        }
    }

    /**
     * Retrieves the server's internal IP address reactively.
     * Uses InetAddress.getLocalHost() to get the host address.
     *
     * @return A Mono emitting the server's internal IP address or "unknown" on failure.
     */
    public static Mono<String> getServerInnerIP() {
        return Mono.fromCallable(() -> InetAddress.getLocalHost().getHostAddress())
                .subscribeOn(Schedulers.boundedElastic())
                .doOnNext(ip -> log.debug("Retrieved server inner IP: {}", ip))
                .onErrorResume(UnknownHostException.class, e -> {
                    log.error("Failed to retrieve server inner IP", e);
                    return Mono.just(UNKNOWN_IP);
                });
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
