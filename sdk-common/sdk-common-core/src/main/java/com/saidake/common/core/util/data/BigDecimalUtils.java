package com.saidake.common.core.util.data;

import lombok.extern.slf4j.Slf4j;

import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.ParseException;

/**
 * 精确计算
 */
@Slf4j
public final class BigDecimalUtils {

	/**
	 * 默认除法运算精度
	 */
	public static final int DEFAULT_DIV_SCALE_2 = 2;
	public static final int DEFAULT_DIV_SCALE_4 = 4;

	/**
	 * 提供精确的加法运算。
	 *
	 * @param v1
	 * @param v2
	 * @return 两个参数的和
	 */
	public static BigDecimal add(Double v1, Double v2) {
		BigDecimal b1 = BigDecimal.valueOf(v1);
		BigDecimal b2 = BigDecimal.valueOf(v2);
		return b1.add(b2).setScale(DEFAULT_DIV_SCALE_2, BigDecimal.ROUND_HALF_EVEN);
	}


	/**
	 * 提供精确的减法运算。
	 *
	 * @param v1
	 * @param v2
	 * @return 两个参数的差
	 */
	public static BigDecimal subtract(Double v1, Double v2) {
		BigDecimal b1 = BigDecimal.valueOf(v1);
		BigDecimal b2 = BigDecimal.valueOf(v2);
		return b1.subtract(b2).setScale(DEFAULT_DIV_SCALE_2, BigDecimal.ROUND_HALF_EVEN);
	}


	/**
	 * 提供精确的乘法运算。
	 *
	 * @param v1
	 * @param v2
	 * @return 两个参数的积
	 */
	public static BigDecimal multiply(Double v1, Double v2) {
		return multiply(v1, v2, DEFAULT_DIV_SCALE_2);
	}

	public static BigDecimal multiply(Double v1, Double v2, int scale) {
		return multiply(v1, v2, scale, BigDecimal.ROUND_HALF_EVEN);
	}

	public static BigDecimal multiply(Double v1, Double v2, int scale, int roundMode) {
		if (scale < 0) {
			throw new IllegalArgumentException("精度指定错误，请指定一个 >= 0 的精度");
		}
		BigDecimal b1 = BigDecimal.valueOf(v1);
		BigDecimal b2 = BigDecimal.valueOf(v2);
		BigDecimal d;
		try {
			d = b1.multiply(b2).setScale(scale, roundMode);
		} catch (Exception e) {
			throw new ArithmeticException("被除数不能为0!!");
		}
		return d;
	}


	/**
	 * 提供（相对）精确的除法运算，当发生除不尽的情况时，精确到 小数点以后2位，以后的数字四舍五入,舍入模式采用ROUND_HALF_EVEN
	 *
	 * @param v1
	 * @param v2
	 * @return 两个Double类型参数的商
	 */
	public static BigDecimal divide(Double v1, Double v2) {
		return divide(v1, v2, DEFAULT_DIV_SCALE_2);
	}

	/**
	 * 提供（相对）精确的除法运算，当发生除不尽的情况时，由scale参数指 定精度，以后的数字四舍五入。舍入模式采用用户指定舍入模式
	 *
	 * @param v1
	 * @param v2
	 * @param scale 表示需要精确到小数点后几位
	 * @return 两个Double类型参数的商
	 */
	public static BigDecimal divide(Double v1, Double v2, int scale) {
		return divide(v1, v2, scale, BigDecimal.ROUND_HALF_EVEN);
	}

	/**
	 * 提供（相对）精确的除法运算。当发生除不尽的情况时，由scale参数指 定精度，以后的数字四舍五入。舍入模式采用用户指定舍入模式
	 *
	 * @param v1
	 * @param v2
	 * @param scale     表示需要精确到小数点后几位
	 * @param roundMode 表示用户指定的舍入模式
	 * @return 两个Double类型参数的商
	 */
	public static BigDecimal divide(Double v1, Double v2, int scale, int roundMode) {
		if (scale < 0) {
			throw new IllegalArgumentException("精度指定错误，请指定一个 >= 0 的精度");
		}
		BigDecimal b1 = BigDecimal.valueOf(v1);
		BigDecimal b2 = BigDecimal.valueOf(v2);

		BigDecimal d;
		try {
			d = b1.divide(b2, scale, roundMode);
		} catch (Exception e) {
			throw new ArithmeticException("被除数不能为0!!");
		}
		return d;
	}


	/**
	 * 将数字格式的字符串里面，有逗号去掉
	 *
	 * @param str=5,000,000.00
	 * @return 去掉逗号后为 5000000.00
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
			log.error("异常信息为：",e);
		}
//		return BigDecimal.valueOf(Double.valueOf(num.toString())).toString();
		if(num != null){
			return BigDecimal.valueOf(Double.valueOf(num.toString())).toString();
		}else {
			return "";
		}
	}

}
