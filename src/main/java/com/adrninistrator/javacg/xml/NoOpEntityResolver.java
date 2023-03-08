package com.adrninistrator.javacg.xml;

import org.xml.sax.EntityResolver;
import org.xml.sax.InputSource;

import java.io.ByteArrayInputStream;
import java.nio.charset.StandardCharsets;

/**
 * @author adrninistrator
 * @date 2023/3/7
 * @description:
 */
public class NoOpEntityResolver implements EntityResolver {
    public InputSource resolveEntity(String publicId, String systemId) {
        return new InputSource(new ByteArrayInputStream("".getBytes(StandardCharsets.UTF_8)));
    }
}