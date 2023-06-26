package com.saidake.common.util.data;


import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.NumberFormat;

/***
 *
 * 金额
 *
 * 如果需要精确计算，必须用String来够造BigDecimal
 *
 * Java里面的商业计算，不能用float和double，因为他们无法 进行精确计算。
 * 但是Java的设计者给编程人员提供了一个很有用的类BigDecimal，
 * 他可以完善float和double类无法进行精确计算的缺憾。
 * BigDecimal类位于java.maths类包下。
 * 它的构造函数很多，最常用的:
 * BigDecimal(double val)
 * BigDecimal(String str)
 * BigDecimal(BigInteger val)
 * BigDecimal(BigInteger unscaledVal, int scale)
 *
 *
 *
 */
public class AmountUtil {

    /**
     * 2位小数
     */
    public final static Integer DECIMALS = 2;

    /**
     * 百分比例
     */
    public final static Integer RATIO = 100;

    /***
     * 保留2位小数
     * 四舍五入
     *
     * @return
     * 返回一个double类型的2位小数
     */
    public static Double get2Double(Double doubleVal, int scale) {
        if (null == doubleVal) {
            doubleVal = new Double(0);
        }
        return new BigDecimal(doubleVal).setScale(scale, BigDecimal.ROUND_HALF_UP).doubleValue();
    }

    /***
     * 格式化Double类型并保留scale位小数
     * 四舍五入
     * @param doubleVal
     * @param scale
     * scale必须为大于0的正整数，不能等于0
     * @return
     */
    public static String formatBy2Scale(Double doubleVal, int scale) {
        if (null == doubleVal) {
            doubleVal = new Double(0);
        }
        StringBuffer sbStr = new StringBuffer("0.");
        for (int i = 0; i < scale; i++) {
            sbStr.append("0");
        }
        DecimalFormat myformat = new DecimalFormat(sbStr.toString());
        return myformat.format(doubleVal);
    }

    /***
     * Double类型相加 <font color="red">+</font><br/>
     * ROUND_HALF_UP <font color="red">四舍五入</font><br/>
     * @param val1
     *
     * @param val2
     *
     * @param scale
     * <font color="red">保留scale位小数</font><br/>
     * @return
     */
    public static Double add(Double val1, Double val2, int scale) {
        if (null == val1) {
            val1 = new Double(0);
        }
        if (null == val2) {
            val2 = new Double(0);
        }
        return new BigDecimal(Double.toString(val1)).add(new BigDecimal(Double.toString(val2))).setScale(scale, BigDecimal.ROUND_HALF_UP).doubleValue();
    }

    public static BigDecimal add(BigDecimal val1, BigDecimal val2, int scale) {
        if (null == val1) {
            val1 = new BigDecimal(0);
        }
        if (null == val2) {
            val2 = new BigDecimal(0);
        }
        return val1.add(val2).setScale(scale, BigDecimal.ROUND_HALF_UP);
    }

    /***
     * Double类型相减 <font color="red">—</font><br/>
     * ROUND_HALF_UP <font color="red">四舍五入</font><br/>
     * @param val1
     *
     * @param val2
     *
     * @param scale
     * <font color="red">保留scale位小数</font><br/>
     * @return
     */
    public static Double subtract(Double val1, Double val2, int scale) {
        if (null == val1) {
            val1 = new Double(0);
        }
        if (null == val2) {
            val2 = new Double(0);
        }
        return new BigDecimal(Double.toString(val1)).subtract(new BigDecimal(Double.toString(val2))).setScale(scale, BigDecimal.ROUND_HALF_UP).doubleValue();
    }

    /***
     * Double类型相减 <font color="red">—</font><br/>
     * ROUND_HALF_UP <font color="red">四舍五入</font><br/>
     * @param val1
     *
     * @param val2
     *
     * @param scale
     * <font color="red">保留scale位小数</font><br/>
     * @return
     */
    public static BigDecimal subtract(BigDecimal val1, BigDecimal val2, int scale) {
        if (null == val1) {
            val1 = new BigDecimal(0);
        }
        if (null == val2) {
            val2 = new BigDecimal(0);
        }
        return val1.subtract(val2).setScale(scale, BigDecimal.ROUND_HALF_UP);
    }

    /***
     * Double类型相乘 <font color="red">*</font><br/>
     * ROUND_HALF_UP <font color="red">四舍五入</font><br/>
     * @param val1
     *
     * @param val2
     *
     * @param scale
     * <font color="red">保留scale位小数</font><br/>
     * @return
     */
    public static Double multiply(Double val1, Double val2, int scale) {
        if (null == val1) {
            val1 = new Double(0);
        }
        if (null == val2) {
            val2 = new Double(0);
        }
        return new BigDecimal(Double.toString(val1)).multiply(new BigDecimal(Double.toString(val2))).setScale(scale, BigDecimal.ROUND_HALF_UP).doubleValue();
    }

    public static BigDecimal multiply(BigDecimal val1, BigDecimal val2, int scale) {
        if (null == val1) {
            val1 = new BigDecimal(0);
        }
        if (null == val2) {
            val2 = new BigDecimal(0);
        }
        return val1.multiply(val2).setScale(scale, BigDecimal.ROUND_HALF_UP);
    }

    public static BigDecimal multiplyIntegral(BigDecimal val1, int scale, BigDecimal val2, String round) {
        if (null == val1) {
            val1 = new BigDecimal(0);
        }
        if (null == val2) {
            val2 = new BigDecimal(0);
        }

        int type = BigDecimal.ROUND_HALF_UP;

        if (round.equals("DOWN")) {
            type = BigDecimal.ROUND_HALF_DOWN;
        }

        return val1.multiply(val2).setScale(scale, BigDecimal.ROUND_HALF_UP);
    }

    public static BigDecimal divideIntegral(BigDecimal val1, BigDecimal val2, String round) {
        if (null == val1) {
            val1 = new BigDecimal(0);
        }
        if (null == val2) {
            val2 = new BigDecimal(0);
        }

        int type = BigDecimal.ROUND_HALF_UP;

        if (round.equals("DOWN")) {
            type = BigDecimal.ROUND_HALF_DOWN;
        }
        return val1.divide(val2, 2, BigDecimal.ROUND_HALF_UP);
    }

    /***
     * Double类型相除 <font color="red">/</font><br/>
     * ROUND_HALF_UP <font color="red">四舍五入</font><br/>
     * @param val1
     * @param val2
     * @param scale
     * <font color="red">保留scale位小数</font><br/>
     * @return
     */
    public static Double divide(Double val1, Double val2, int scale) {
        if (null == val1) {
            val1 = new Double(0);
        }
        if (null == val2 || val2 == 0) {
            val2 = new Double(1);
        }
        return new BigDecimal(Double.toString(val1)).divide(new BigDecimal(Double.toString(val2)), 2, BigDecimal.ROUND_HALF_UP).doubleValue();
    }

    public static BigDecimal divide(BigDecimal val1, BigDecimal val2, int scale) {
        if (null == val1) {
            val1 = new BigDecimal(0);
        }
        if (null == val2 || val2.equals(new BigDecimal(0))) {
            val2 = new BigDecimal(1);
        }
        return val1.divide(val2, scale, BigDecimal.ROUND_HALF_UP);
    }

    /***
     * Double类型取余    <font color="red">%</font><br/>
     * ROUND_HALF_UP <font color="red">四舍五入</font><br/>
     * @param val1
     * @param val2
     * @param scale
     * <font color="red">保留scale位小数</font><br/>
     * @return
     */
    public static int divideAndRemainder(Double val1, Double val2, int scale) {
        if (null == val1) {
            val1 = new Double(0);
        }
        if (null == val2 || val2 == 0) {
            val2 = new Double(1);
        }
        return new BigDecimal(Double.toString(val1)).divideAndRemainder(new BigDecimal(Double.toString(val2)))[1].setScale(scale, BigDecimal.ROUND_HALF_UP).intValue();
    }

    /***
     * 格式化Double类型数据
     *
     * @param val
     * @param fmt
     * NumberFormat currency = NumberFormat.getCurrencyInstance(); //建立货币格式化引用
     * NumberFormat percent = NumberFormat.getPercentInstance(); //建立百分比格式化引用
     * @param maximumFractionDigits
     * 如果是百分比 设置小数位数（四舍五入）
     * @return
     */
    public static String formatByNumberFormat(Double val, NumberFormat fmt, int maximumFractionDigits) {
        if (fmt.equals(NumberFormat.getPercentInstance())) {
            fmt.setMaximumFractionDigits(maximumFractionDigits); //百分比小数点最多3位
        }
        return fmt.format(val);

    }

    /***
     * 比较大小
     * -1、0、1，即左边比右边数大，返回1，相等返回0，比右边小返回-1。
     * @return
     */
    public static int compareTo(Double val1, Double val2) {
        if (null == val1) {
            val1 = new Double(0);
        }
        if (null == val2) {
            val2 = new Double(0);
        }
        return new BigDecimal(val1).compareTo(new BigDecimal(val2));
    }

    public static int compareTo(BigDecimal val1, BigDecimal val2) {
        return val1.compareTo(val2);
    }

    private static final Integer COMMODITY_COMMISSION = 0;
    private static final Integer COMMODITY_PLATFROM = 1;

    /**
     * 计算佣金
     * 商户佣金 = （商户零售价-供货物价）* 比例
     */
    public static BigDecimal calculateCommissionPrice(BigDecimal sellingPrice, BigDecimal basePrice, BigDecimal ratio) {

        BigDecimal r = AmountUtil.divide(ratio, new BigDecimal(AmountUtil.RATIO), AmountUtil.DECIMALS);

        BigDecimal subtractPrice = AmountUtil.subtract(sellingPrice, basePrice, AmountUtil.DECIMALS);

        BigDecimal price = AmountUtil.multiply(subtractPrice, r, AmountUtil.DECIMALS);

        System.out.println("(商户零售价-供货物价) * 比例: " + (sellingPrice + "-" + basePrice) + "*" + r + "=" + price);

        return price;
    }

    /**
     * 平台供货价
     * 平台供货价 = 供货价 + （商户零售价-供货物价）* 比例
     */
    public static BigDecimal calculatePlatFromPrice(BigDecimal sellingPrice, BigDecimal basePrice, BigDecimal ratio) {

        BigDecimal price = calculateCommissionPrice(sellingPrice, basePrice, ratio);

        BigDecimal platFromPrice = AmountUtil.add(basePrice, price, AmountUtil.DECIMALS);

        System.out.println("供货物价+(商户零售价-供货物价) * 比例: " + basePrice + "+" + (sellingPrice + "-" + basePrice) + "*" + ratio + "/100=" + platFromPrice);

        return platFromPrice;
    }

    /**
     * 特惠价 = 供货价 * （1 + A供应商加价百分比），四舍五入保留两位小数
     */
    public static BigDecimal calculateSpecPrice(BigDecimal basePrice, BigDecimal ratio) {

        BigDecimal r = AmountUtil.divide(ratio, new BigDecimal(AmountUtil.RATIO), AmountUtil.DECIMALS).add(new BigDecimal("1"));

        BigDecimal platFromPrice = AmountUtil.multiply(basePrice, r, AmountUtil.DECIMALS);

        System.out.println("供货价 * （1 + A供应商加价百分比）: " + basePrice + "*" + "(1" + "+" + ratio + "/100)=" + platFromPrice);

        return platFromPrice;
    }

    /**
     * 折扣 = （售价/市场价）* 10
     *
     * @param sellingPrice  售价
     * @param originalPrice 市场价
     * @return Double类型的结果
     * @description 打几折
     */
    public static Double discount(Double sellingPrice, Double originalPrice) {
        return bigDecimalDiscount(sellingPrice, originalPrice)
                .doubleValue();
    }

    /**
     * 折扣 = （售价/市场价）* 10
     *
     * @param sellingPrice  售价
     * @param originalPrice 市场价
     * @return BigDecimal类型的结果
     * @description 打几折
     */
    public static BigDecimal bigDecimalDiscount(Double sellingPrice, Double originalPrice) {
        int discountCompare = compareTo(sellingPrice, originalPrice);
        if (discountCompare >= 0) return BigDecimal.valueOf(10);
        return BigDecimalUtils.multiply(sellingPrice, Double.valueOf(10))
                .divide(BigDecimal.valueOf(originalPrice), BigDecimalUtils.DEFAULT_DIV_SCALE_2, BigDecimal.ROUND_HALF_EVEN);
    }

    /**
     * 利润率 % = （（售价-进货价）* 100 / 进货价 ）%
     *
     * @param sellingPrice 售价
     * @param basePrice    进货价
     * @return Double 类型的结果
     * @description 利润率 xxx%, 此方法只返回 xxx
     */
    public static Double profit(Double sellingPrice, Double basePrice) {
        return bigDecimalProfit(sellingPrice, basePrice)
                .doubleValue();
    }

    /**
     * 利润率 % = （（售价-进货价）* 100 / 进货价 ）%
     * @param sellingPrice 售价
     * @param basePrice    进货价
     * @return BigDecimal 类型
     * @description 利润率 xxx%, 此方法只返回 xxx
     */

    public static BigDecimal bigDecimalProfit(Double sellingPrice, Double basePrice) {
        int profitCompare = compareTo(sellingPrice, basePrice);
        if (profitCompare <= 0) return BigDecimal.valueOf(0);
        return BigDecimalUtils.subtract(sellingPrice, basePrice)
                .multiply(BigDecimal.valueOf(100))
                .divide(BigDecimal.valueOf(basePrice), BigDecimalUtils.DEFAULT_DIV_SCALE_2, BigDecimal.ROUND_HALF_EVEN);
    }

}

