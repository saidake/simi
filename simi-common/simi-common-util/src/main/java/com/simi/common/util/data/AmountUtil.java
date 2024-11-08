package com.simi.common.util.data;

import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.NumberFormat;

/***
 * Amount Utility Class
 * <p>
 * For accurate calculations, always use String to construct BigDecimal.
 * Java's BigDecimal class helps to perform precise business calculations
 * without the inaccuracies of float and double.
 */
public class AmountUtil {

    /**
     * Decimal places (2 decimal points)
     */
    public static final Integer DECIMALS = 2;

    /**
     * Percentage ratio (100)
     */
    public static final Integer RATIO = 100;

    /***
     * Rounds to 2 decimal places
     *
     * @return Returns a Double with 2 decimal places
     */
    public static Double get2Double(Double doubleVal, int scale) {
        if (doubleVal == null) {
            doubleVal = 0.0;
        }
        return new BigDecimal(doubleVal).setScale(scale, BigDecimal.ROUND_HALF_UP).doubleValue();
    }

    /***
     * Formats a Double value and keeps the specified number of decimal places
     *
     * @param doubleVal
     * @param scale
     * @return Formatted string
     */
    public static String formatBy2Scale(Double doubleVal, int scale) {
        if (doubleVal == null) {
            doubleVal = 0.0;
        }
        StringBuilder sbStr = new StringBuilder("0.");
        sbStr.append("0".repeat(scale));
        DecimalFormat myFormat = new DecimalFormat(sbStr.toString());
        return myFormat.format(doubleVal);
    }

    /***
     * Adds two Double values
     */
    public static Double add(Double val1, Double val2, int scale) {
        if (val1 == null) {
            val1 = 0.0;
        }
        if (val2 == null) {
            val2 = 0.0;
        }
        return new BigDecimal(val1.toString()).add(new BigDecimal(val2.toString())).setScale(scale, BigDecimal.ROUND_HALF_UP).doubleValue();
    }

    public static BigDecimal add(BigDecimal val1, BigDecimal val2, int scale) {
        if (val1 == null) {
            val1 = BigDecimal.ZERO;
        }
        if (val2 == null) {
            val2 = BigDecimal.ZERO;
        }
        return val1.add(val2).setScale(scale, BigDecimal.ROUND_HALF_UP);
    }

    /***
     * Subtracts two Double values
     */
    public static Double subtract(Double val1, Double val2, int scale) {
        if (val1 == null) {
            val1 = 0.0;
        }
        if (val2 == null) {
            val2 = 0.0;
        }
        return new BigDecimal(val1.toString()).subtract(new BigDecimal(val2.toString())).setScale(scale, BigDecimal.ROUND_HALF_UP).doubleValue();
    }

    public static BigDecimal subtract(BigDecimal val1, BigDecimal val2, int scale) {
        if (val1 == null) {
            val1 = BigDecimal.ZERO;
        }
        if (val2 == null) {
            val2 = BigDecimal.ZERO;
        }
        return val1.subtract(val2).setScale(scale, BigDecimal.ROUND_HALF_UP);
    }

    /***
     * Multiplies two Double values
     */
    public static Double multiply(Double val1, Double val2, int scale) {
        if (val1 == null) {
            val1 = 0.0;
        }
        if (val2 == null) {
            val2 = 0.0;
        }
        return new BigDecimal(val1.toString()).multiply(new BigDecimal(val2.toString())).setScale(scale, BigDecimal.ROUND_HALF_UP).doubleValue();
    }

    public static BigDecimal multiply(BigDecimal val1, BigDecimal val2, int scale) {
        if (val1 == null) {
            val1 = BigDecimal.ZERO;
        }
        if (val2 == null) {
            val2 = BigDecimal.ZERO;
        }
        return val1.multiply(val2).setScale(scale, BigDecimal.ROUND_HALF_UP);
    }

    /***
     * Divides two Double values
     */
    public static Double divide(Double val1, Double val2, int scale) {
        if (val1 == null) {
            val1 = 0.0;
        }
        if (val2 == null || val2 == 0.0) {
            val2 = 1.0;
        }
        return new BigDecimal(val1.toString()).divide(new BigDecimal(val2.toString()), scale, BigDecimal.ROUND_HALF_UP).doubleValue();
    }

    public static BigDecimal divide(BigDecimal val1, BigDecimal val2, int scale) {
        if (val1 == null) {
            val1 = BigDecimal.ZERO;
        }
        if (val2 == null || val2.equals(BigDecimal.ZERO)) {
            val2 = BigDecimal.ONE;
        }
        return val1.divide(val2, scale, BigDecimal.ROUND_HALF_UP);
    }

    /***
     * Remainder operation for Double values
     */
    public static int divideAndRemainder(Double val1, Double val2, int scale) {
        if (val1 == null) {
            val1 = 0.0;
        }
        if (val2 == null || val2 == 0.0) {
            val2 = 1.0;
        }
        return new BigDecimal(val1.toString()).divideAndRemainder(new BigDecimal(val2.toString()))[1].setScale(scale, BigDecimal.ROUND_HALF_UP).intValue();
    }

    /***
     * Formats a Double using a specified NumberFormat
     */
    public static String formatByNumberFormat(Double val, NumberFormat fmt, int maxFractionDigits) {
        fmt.setMaximumFractionDigits(maxFractionDigits);
        return fmt.format(val);
    }

    /***
     * Compares two Double values
     */
    public static int compareTo(Double val1, Double val2) {
        if (val1 == null) {
            val1 = 0.0;
        }
        if (val2 == null) {
            val2 = 0.0;
        }
        return new BigDecimal(val1).compareTo(new BigDecimal(val2));
    }

    public static int compareTo(BigDecimal val1, BigDecimal val2) {
        return val1.compareTo(val2);
    }

    /**
     * Calculates commission price
     */
    public static BigDecimal calculateCommissionPrice(BigDecimal sellingPrice, BigDecimal basePrice, BigDecimal ratio) {
        BigDecimal r = divide(ratio, BigDecimal.valueOf(RATIO), DECIMALS);
        BigDecimal subtractPrice = subtract(sellingPrice, basePrice, DECIMALS);
        return multiply(subtractPrice, r, DECIMALS);
    }

    /**
     * Calculates platform price
     */
    public static BigDecimal calculatePlatFromPrice(BigDecimal sellingPrice, BigDecimal basePrice, BigDecimal ratio) {
        BigDecimal commission = calculateCommissionPrice(sellingPrice, basePrice, ratio);
        return add(basePrice, commission, DECIMALS);
    }
}
