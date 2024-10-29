package com.gym.fit_power.util;

import org.apache.commons.lang3.RandomStringUtils;

public class CodeGeneratorUtil {

    private static final int LONGITUD_CODIGO = 8;
    private static final String CARACTERES_CODIGO = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";

    public static String generarCodigo() {
        return RandomStringUtils.random(LONGITUD_CODIGO, CARACTERES_CODIGO);
    }

    public static String generarCodigo(int longitud) {
        return RandomStringUtils.random(longitud, CARACTERES_CODIGO);
    }

    public static String generarCodigo(String caracteres) {
        return RandomStringUtils.random(LONGITUD_CODIGO, caracteres);
    }

    public static String generarCodigo(int longitud, String caracteres) {
        return RandomStringUtils.random(longitud, caracteres);
    }

}
