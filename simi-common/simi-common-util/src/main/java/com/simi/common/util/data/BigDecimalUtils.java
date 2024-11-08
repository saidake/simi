package com.simi.common.util.data;

import lombok.extern.slf4j.Slf4j;

import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.ParseException;

/**
 * Utility class for precise arithmetic calculations using BigDecimal.
 */
@Slf4j
public final class BigDecimalUtils {

	/**
	 * Default scale for division operations, using 2 decimal places.
	 */
	public static final int DEFAULT_DIV_SCALE_2 = 2;

	/**
	 * Default scale for division operations, using 4 decimal places.
	 */
	public static final int DEFAULT_DIV_SCALE_4 = 4;

	/**
	 * Provides precise addition of two Double values.
	 *
	 * @param v1 The first value to add.
	 * @param v2 The second value to add.
	 * @return The sum of v1 and v2, rounded to 2 decimal places using HALF_EVEN rounding mode.
	 */
	public static BigDecimal add(Double v1, Double v2) {
		BigDecimal b1 = BigDecimal.valueOf(v1);
		BigDecimal b2 = BigDecimal.valueOf(v2);
		return b1.add(b2).setScale(DEFAULT_DIV_SCALE_2, BigDecimal.ROUND_HALF_EVEN);
	}

	/**
	 * Provides precise subtraction of two Double values.
	 *
	 * @param v1 The value to subtract from (minuend).
	 * @param v2 The value to be subtracted (subtrahend).
	 * @return The difference between v1 and v2, rounded to 2 decimal places using HALF_EVEN rounding mode.
	 */
	public static BigDecimal subtract(Double v1, Double v2) {
		BigDecimal b1 = BigDecimal.valueOf(v1);
		BigDecimal b2 = BigDecimal.valueOf(v2);
		return b1.subtract(b2).setScale(DEFAULT_DIV_SCALE_2, BigDecimal.ROUND_HALF_EVEN);
	}

	/**
	 * Provides precise multiplication of two Double values.
	 *
	 * @param v1 The first value to multiply.
	 * @param v2 The second value to multiply.
	 * @return The product of v1 and v2, rounded to 2 decimal places using HALF_EVEN rounding mode.
	 */
	public static BigDecimal multiply(Double v1, Double v2) {
		return multiply(v1, v2, DEFAULT_DIV_SCALE_2);
	}

	/**
	 * Provides precise multiplication of two Double values with specified scale.
	 *
	 * @param v1    The first value to multiply.
	 * @param v2    The second value to multiply.
	 * @param scale The number of decimal places to round to.
	 * @return The product of v1 and v2, rounded to the specified scale using HALF_EVEN rounding mode.
	 */
	public static BigDecimal multiply(Double v1, Double v2, int scale) {
		return multiply(v1, v2, scale, BigDecimal.ROUND_HALF_EVEN);
	}

	/**
	 * Provides precise multiplication of two Double values with specified scale and rounding mode.
	 *
	 * @param v1        The first value to multiply.
	 * @param v2        The second value to multiply.
	 * @param scale     The number of decimal places to round to.
	 * @param roundMode The rounding mode to apply.
	 * @return The product of v1 and v2, rounded to the specified scale and using the specified rounding mode.
	 */
	public static BigDecimal multiply(Double v1, Double v2, int scale, int roundMode) {
		if (scale < 0) {
			throw new IllegalArgumentException("Invalid scale value. Scale must be greater than or equal to 0.");
		}
		BigDecimal b1 = BigDecimal.valueOf(v1);
		BigDecimal b2 = BigDecimal.valueOf(v2);
		BigDecimal result;
		try {
			result = b1.multiply(b2).setScale(scale, roundMode);
		} catch (Exception e) {
			throw new ArithmeticException("Division by zero error!");
		}
		return result;
	}

	/**
	 * Provides precise division of two Double values, rounded to 2 decimal places using HALF_EVEN rounding mode.
	 *
	 * @param v1 The dividend.
	 * @param v2 The divisor.
	 * @return The quotient of v1 and v2, rounded to 2 decimal places.
	 */
	public static BigDecimal divide(Double v1, Double v2) {
		return divide(v1, v2, DEFAULT_DIV_SCALE_2);
	}

	/**
	 * Provides precise division of two Double values with specified scale, using HALF_EVEN rounding mode.
	 *
	 * @param v1    The dividend.
	 * @param v2    The divisor.
	 * @param scale The number of decimal places to round to.
	 * @return The quotient of v1 and v2, rounded to the specified scale.
	 */
	public static BigDecimal divide(Double v1, Double v2, int scale) {
		return divide(v1, v2, scale, BigDecimal.ROUND_HALF_EVEN);
	}

	/**
	 * Provides precise division of two Double values with specified scale and rounding mode.
	 *
	 * @param v1        The dividend.
	 * @param v2        The divisor.
	 * @param scale     The number of decimal places to round to.
	 * @param roundMode The rounding mode to apply.
	 * @return The quotient of v1 and v2, rounded to the specified scale and using the specified rounding mode.
	 */
	public static BigDecimal divide(Double v1, Double v2, int scale, int roundMode) {
		if (scale < 0) {
			throw new IllegalArgumentException("Invalid scale value. Scale must be greater than or equal to 0.");
		}
		BigDecimal b1 = BigDecimal.valueOf(v1);
		BigDecimal b2 = BigDecimal.valueOf(v2);

		BigDecimal result;
		try {
			result = b1.divide(b2, scale, roundMode);
		} catch (Exception e) {
			throw new ArithmeticException("Division by zero error!");
		}
		return result;
	}

	/**
	 * Removes comma separators from a formatted numeric string.
	 *
	 * @param str A numeric string with comma separators (e.g., "5,000,000.00").
	 * @return A string with comma separators removed (e.g., "5000000.00"), or an empty string if parsing fails.
	 */
	public static String parseDouStr(String str) {
		DecimalFormatSymbols dfs = new DecimalFormatSymbols();
		dfs.setDecimalSeparator('.');
		dfs.setGroupingSeparator(',');
		dfs.setMonetaryDecimalSeparator('.');
		DecimalFormat df = new DecimalFormat("###,###.##", dfs);

		Number num = null;
		try {
			num = df.parse(str);
		} catch (ParseException e) {
			log.error("Parsing error: ", e);
		}
		return num != null ? BigDecimal.valueOf(Double.parseDouble(num.toString())).toString() : "";
	}

}
