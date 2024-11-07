package com.simi.AAAconfig;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.commons.io.IOUtils;

import jakarta.servlet.ReadListener;
import jakarta.servlet.ServletInputStream;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletRequestWrapper;
import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Map;

public class TestServletRequestWrapper extends HttpServletRequestWrapper {

    private final ServletInputStream inputStream;

    private BufferedReader reader;

    private String requestBodyLine;

    /**
     * Constructs a request object wrapping the given request.
     *
     * @param request The request to wrap
     * @throws IllegalArgumentException if the request is null
     */
    public TestServletRequestWrapper(HttpServletRequest request) throws IOException {
        super(request);
        request.getParameterNames();
        //读一次 然后缓存 实现同一request多次读取
        byte[] body = IOUtils.toByteArray(request.getInputStream());
        this.inputStream = new RequestCachingInputStream(body);
    }

    @Override
    public ServletInputStream getInputStream() throws IOException {
        if (this.inputStream != null) {
            return this.inputStream;
        }
        return super.getInputStream();
    }

    @Override
    public BufferedReader getReader() throws IOException {
        if (this.reader == null) {
            this.reader = new BufferedReader(new InputStreamReader(getInputStream(), getCharacterEncoding()));
        }
        return this.reader;
    }

    /**
     * 将请求体参数存入Map中
     * @return
     * @throws IOException
     */
    public Map<String, Object> getRequestBodyToMap() throws IOException {
        String jsonInfo = this.getRequestBodyToStr();
        ObjectMapper objectMapper = new ObjectMapper();
        return objectMapper.readValue(jsonInfo,Map.class);
    }

    public String getRequestBodyToStr() throws IOException {
        if (this.requestBodyLine == null) {
            BufferedReader reader = this.getReader();
            StringBuilder builder = new StringBuilder();
            String line = reader.readLine();
            while (line != null) {
                builder.append(line);
                line = reader.readLine();
            }
            requestBodyLine = builder.toString();
            reader.close();
        }
        return requestBodyLine;
    }

    //代理ServletInputStream 内容为当前缓存的bytes
    private static class RequestCachingInputStream extends ServletInputStream {

        private final ByteArrayInputStream is;

        public RequestCachingInputStream(byte[] bytes) {
            this.is = new ByteArrayInputStream(bytes);
        }

        @Override
        public int read() throws IOException {
            return this.is.read();
        }

        @Override
        public boolean isFinished() {
            return this.is.available() == 0;
        }

        @Override
        public boolean isReady() {
            return true;
        }

        @Override
        public void setReadListener(ReadListener readlistener) {
        }
    }
}
