package com.gym.fit_power.util;

import jakarta.persistence.AttributeConverter;
import jakarta.persistence.Converter;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.GregorianCalendar;


@Converter (autoApply = true)
public class MyGregorianCalendarConverter implements AttributeConverter<GregorianCalendar,String> {

    private static final String DATE_FORMAT = "dd-MM-yyyy";
    private final SimpleDateFormat sdf = new SimpleDateFormat(DATE_FORMAT);

    @Override
    public String convertToDatabaseColumn(GregorianCalendar attribute) {
        return attribute == null ? null : sdf.format(attribute.getTime());
    }

    @Override
    public GregorianCalendar convertToEntityAttribute(String dbData) {
        if (dbData == null || dbData.isEmpty()) {
            return null;
        }
        GregorianCalendar calendar = new GregorianCalendar();
        try {
            calendar.setTime(sdf.parse(dbData));
        } catch (ParseException e) {
            e.printStackTrace();
            return null;
        }
        return calendar;
    }
}
