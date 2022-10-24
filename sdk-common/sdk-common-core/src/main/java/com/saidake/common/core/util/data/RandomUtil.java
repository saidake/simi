package com.saidake.common.core.util.data;

import lombok.experimental.UtilityClass;
import org.springframework.beans.BeanWrapper;
import org.springframework.beans.BeanWrapperImpl;

import java.beans.PropertyDescriptor;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


/**
 * 随机工具类
 */
@UtilityClass
public class RandomUtil {
    private static String ChineseFamilyNameSingle = "赵钱孙李周吴郑王冯陈褚卫蒋沈韩杨朱秦尤许何吕施张孔曹严华金魏陶姜戚谢邹喻水云苏潘葛奚范彭郎鲁韦昌马苗凤花方俞任袁柳鲍史唐费岑薛雷贺倪汤滕殷罗毕郝邬安常乐于时傅卞齐康伍余元卜顾孟平"
            + "黄和穆萧尹姚邵湛汪祁毛禹狄米贝明臧计成戴宋茅庞熊纪舒屈项祝董粱杜阮席季麻强贾路娄危江童颜郭梅盛林刁钟徐邱骆高夏蔡田胡凌霍万柯卢莫房缪干解应宗丁宣邓郁单杭洪包诸左石崔吉"
            + "龚程邢滑裴陆荣翁荀羊甄家封芮储靳邴松井富乌焦巴弓牧隗山谷车侯伊宁仇祖武符刘景詹束龙叶幸司韶黎乔苍双闻莘劳逄姬冉宰桂牛寿通边燕冀尚农温庄晏瞿茹习鱼容向古戈终居衡步都耿满弘国文东殴沃曾关红游盖益桓公晋楚闫";
    private static String ChineseFamilyNameDouble = "欧阳太史端木上官司马东方独孤南宫万俟闻人夏侯诸葛尉迟公羊赫连澹台皇甫宗政濮阳公冶太叔申屠公孙慕容仲孙钟离长孙宇文司徒鲜于司空闾丘子车亓官司寇巫马公西颛孙壤驷公良漆雕乐正宰父谷梁拓跋夹谷轩辕令狐段干百里呼延东郭南门羊舌微生公户公玉公仪梁丘公仲公上公门公山公坚左丘公伯西门公祖第五公乘贯丘公皙南荣东里东宫仲长子书子桑即墨达奚褚师吴铭";
    private static String girlName = "秀娟英华慧巧美娜静淑惠珠翠雅芝玉萍红娥玲芬芳燕彩春菊兰凤洁梅琳素云莲真环雪荣爱妹霞香月莺媛艳瑞凡佳嘉琼勤珍贞莉桂娣叶璧璐娅琦晶妍茜秋珊莎锦黛青倩婷姣婉娴瑾颖露瑶怡婵雁蓓纨仪荷丹蓉眉君琴蕊薇菁梦岚苑婕馨瑗琰韵融园艺咏卿聪澜纯毓悦昭冰爽琬茗羽希宁欣飘育滢馥筠柔竹霭凝晓欢霄枫芸菲寒伊亚宜可姬舒影荔枝思丽";
    private static String boyName = "伟刚勇毅俊峰强军平保东文辉力明永健世广志义兴良海山仁波宁贵福生龙元全国胜学祥才发武新利清飞彬富顺信子杰涛昌成康星光天达安岩中茂进林有坚和彪博诚先敬震振壮会思群豪心邦承乐绍功松善厚庆磊民友裕河哲江超浩亮政谦亨奇固之轮翰朗伯宏言若鸣朋斌梁栋维启克伦翔旭鹏泽晨辰士以建家致树炎德行时泰盛雄琛钧冠策腾楠榕风航弘";

    private static final String[] EnglishFamilyName = {
            "Aaron","Abel","Abraham","Adam","Adrian","Alva","Alex","Alexander","Alan","Albert","Alfred","Andrew","Andy","Angus","Anthony","Arthur","Austin","Ben","Benson","Bill","Bob","Brandon","Brant","Brent","Brian","Bruce","Carl","Cary","Caspar","Charles","Cheney","Chris","Christian","Christopher","Colin","Cosmo","Daniel","Dennis","Derek","Donald","Douglas","David","Denny","Edgar","Edward","Edwin","Elliott","Elvis","Eric","Evan","Francis","Frank","Franklin","Fred","Gabriel","Gaby","Garfield","Gary","Gavin","George","Gino","Glen","Glendon","Harrison","Hugo","Hunk","Howard","Henry","Ignativs","Ivan","Isaac","Jack","Jackson","Jacob","James","Jason","Jeffery","Jerome","Jerry","Jesse","Jim","Jimmy","Joe","John","Johnny","Joseph","Joshua","Justin","Keith","Ken","Kenneth","Kenny","Kevin","Lance","Larry","Laurent","Lawrence","Leander","Lee","Leo","Leonard","Leopold","Loren","Lori","Lorin","Luke","Marcus","Marcy","Mark","Marks","Mars","Martin","Matthew","Michael","Mike","Neil","Nicholas","Oliver","Oscar","Paul","Patrick","Peter","Philip","Phoebe","Quentin","Randall","Randolph","Randy","Reed","Rex","Richard","Richie","Robert","Robin","Robinson","Rock","Roger","Roy","Ryan","Sam","Sammy","Samuel","Scott","Sean","Shawn","Sidney","Simon","Solomon","Spark","Spencer","Spike","Stanley","Steven","Stuart","Terence","Terry","Timothy","Tommy","Tom","Thomas","Tony","Tyler","Van","Vern","Vernon","Vincent","Warren","Wesley","William"
    };
    private static final String[] EnglishBoyName = {
            "Albert","Kevin","Michael","Taylor","Jackson","Jack","Jimmy","Allen","Martin","Vincent","Charles","Mark","Bill","Vincent","William","Joseph","James","Henry","Gary","Martin","Fred","Gary","William","Charles","Michael","Karl","Bob","John","Thomas","Dean","Paul","Jack","Brooke","Kevin","Louis","John","George","Henry","Benjamin","Robert","Carl","Scott","Tom","Eddy","Kris","Peter","Johnson","Bruce","Robert","Peter","Bill","Joseph","John","Nick","Walt","John","Mark","Sam","Davis","Neil","Carl","Lewis","Billy","Richard","Howard","Allen","Johnny","Robert","Martin","Jeff","Paul","Sam","Francis","Lewis","Stephen","Andy","Scott"
    };
    private static final String[] EnglishGirlName = {
            "Abby","Aimee","Alisa","Angelia","Angelia","Amanda","Anne","Carrie","Kerry","Cassie","Daisy","Fern","Alice","Bunny","Belle","Estelle","Jasmine","Iris","Emily","Ailsa","Aimee","Alice","Alina","Allison","Amanda","Amy","Amber","Anastasia","Stacey","Andrea","Angela","Angelia","Angelina","Ann","Hannah","Anne","Ann","Annie","Ann","Anita","Ann","Ariel","April","Ashley","Aviva","Avivahc","Avivi","Barbara","Beata","Beatrice","Beatrix","Becky","Rebecca","Betty","Elizabeth","Blanche","Bonnie","Brenda","Brandon","Brendan","Camille","Candice","Carina","Carmen","Carol","Caroline","Carry","Carrie","Carol","Caroline","Kerry","Cassandra","Charlene","Caroline","Charlotte","Charlotte","Cherry","Cheryl","Charlotte","Sheryl","Chris","Christine","Kristine","Kris","Christina","Christine","Christine","Christy","Christine","Cindy","Cinderella","Cynthia","Lucinda","Claudia","Clement","Cloris","Connie","Constance","Constance","Cora","Corrine","Crystal","Krystal","Daisy","Daphne","Darcy","Debbie","Deborah","Debra","Deborah","Debra","Demi","Diana","Dolores","Donna","Doris","Edith","Editha","Elaine","Eleanor","Elizabeth","Ella","Ellen","Ellie","Eleanor","Ellen","Estelle","Esther","Eudora","Eva","Eve","Fannie","Frances","Fanny","Fiona","Frances","Francis","Frederica","Frieda","Gina","Angelina","Regina","Gillian","Juliana","Gladys","Claudia","Gloria","Grace","Greta","Margaret","Gwendolyn","Hannah","Helena","Hellen","Helen","Hebe","Heidi","Adalheid","Adelaide","Ingrid","Ishara","Irene","Iris","Ivy","Jacqueline","Jamie","James","Jane","John","Janet","Jane","Jean","Jane","Jessica","Jessee","Jessie","Jasmine","Jessica","Janet","Jennifer","Jenny","Jennie","Jane","Jennifer","Jill","Gillian","Joan","Jane","Joanna","Jocelyn","Josephine","Josie","Josephine","Joy","Joyce","Josephine","Judith","Judy","Judith","Julia","Juliana","Julie","June","Kitty","Catherine","Lareina","Laura","Lawrence","Lena","Helena","Lydia","Lillian","Linda","Lisa","Elizabeth","Liz","Elizabeth","Vanessa","Vicky","Victoria","Victoria","Vivian","Wanda","Wendy","Gwendolyn","Wanda","Winnie","Yolanda","Yvette","Yvonne","Yvonne","Zoey","Zoe","Beenle","Icey","Angle","Fairy","Diana","Rose","Barbie","Moon","Snowy","Snowhite","Christal","Bubles","Colorfully","Purplegrape","Silverdew","Greenle","Star","Fairy","Dreamy","Flower","Magical","Sweety","Yilia","Maria","Nancy","Annabelle","Jodie","Janice","Qearl","Alexandra","Sandra","Sammy","Davis","Sarah","Selina","Sarah","Sharon","Sharon","Shirley","Temple","Susan","Sue","Carina","Cathy","Chris","Vanessa","Nina","Wendy","Gwendolyn","Wanda","Sandy","Emily","Sarah","Brianna","Samantha","Hailey","Ashley","Kaitlyn","Madison","Hannah","Alexis","Carrie","Carol","Caroline","Kerry","Carrie","Fannie","Frances","Fanny","Hebbe","Jasmine","Jessica","Janet","Jessie","Catherine","Kitty","Croesus","Lydia","Liddy","Miranda","Sarah","Selina"
    };


    /**
     * 生成随机英文姓名
     * @return 姓名
     */
    public static String getRandomEnglishName() {
        String currentSex=getRandomSex();
        if("男".equals(currentSex)){
            return getRandomElement(EnglishBoyName)+" "+getRandomElement(EnglishFamilyName);
        }else{
            return getRandomElement(EnglishGirlName)+" "+getRandomElement(EnglishFamilyName);
        }
    }

    /**
     * 生成随机姓名
     * @return 姓名
     */
    public static String getRandomName() {
        String currentSex=getRandomSex();
        return getRandomFamilyName()+getRandomOnlyNameBySex(currentSex);
    }


    /**
     * 功能：随机产生姓氏
     * @return 姓氏
     */
    public static String getRandomFamilyName() {
        String str = "";
        int randNum = new Random().nextInt(2) + 1;
        int strLen = randNum == 1 ? ChineseFamilyNameSingle.length() : ChineseFamilyNameDouble.length();
        int index = new Random().nextInt(strLen);
        if (randNum == 1) {
            str = String.valueOf(ChineseFamilyNameSingle.charAt(index));
        } else {
            str = (index & 1) == 0 ? ChineseFamilyNameDouble.substring(index, index + 2) :
                    ChineseFamilyNameDouble.substring(index - 1, index + 1);
        }
        return str;
    }



    /**
     * 功能：随机产生性别
     *
     * @return
     */
    public static String getRandomSex() {
        int randNum = new Random().nextInt(2) + 1;
        return randNum == 1 ? "男" : "女";
    }

    /**
     * 功能：传入性别参数，依据性别产生名字
     *
     * @param sex
     * @return
     */
    public static String getRandomOnlyNameBySex(String sex) {
        String name;
        int randNum = new Random().nextInt(2) + 1;  // [1,3)
        int strLen = sex.equals("男") ? boyName.length() : girlName.length();
        int index = (randNum & 1) == 0 ? new Random().nextInt(strLen - 1) : new Random().nextInt(strLen);
        name= sex.equals("男") ? boyName.substring(index, index + randNum) :
                girlName.substring(index, index + randNum);
        return name;
    }

    /**
     * 功能：随机产生[0,99)的整数
     *
     * @return
     */
    public static int getRandomAge() {
        return new Random().nextInt(99) ;
    }

    public static Integer getRandomNumFromString(String source) {
        Pattern pattern = Pattern.compile("\\d+");
        Matcher matcher = pattern.matcher(source);
        List<String> stringList=new ArrayList<>();
        while (matcher.find()){
            stringList.add(matcher.group());
        }
        if(stringList.size()==0)return null;
        int nextInt = new Random().nextInt(stringList.size());
        return Integer.valueOf(stringList.get(nextInt));
    }

    /**
     * 功能：随机产生[start, end)的整数
     *
     * @return
     */
    public static int getRandomNum(int start, int end) {
        return new Random().nextInt(end-start)+start;
    }



    //中国移动
    public static final String[] CHINA_MOBILE = {
            "134", "135", "136", "137", "138", "139", "150", "151", "152", "157", "158", "159",
            "182", "183", "184", "187", "188", "178", "147", "172", "198"
    };
    //中国联通
    public static final String[] CHINA_UNICOM = {
            "130", "131", "132", "145", "155", "156", "166", "171", "175", "176", "185", "186", "166"
    };
    //中国电信
    public static final String[] CHINA_TELECOME = {
            "133", "149", "153", "173", "177", "180", "181", "189", "199"
    };


    /**
     * 生成随机手机号
     */
    public static String getRandomPhone() {
        return getRandomPhoneByOp( new Random().nextInt(3));
    }

    /**
     * 生成手机号
     *
     * @param op 0 移动 1 联通 2 电信
     */
    public static String getRandomPhoneByOp(int op) {
        StringBuilder sb = new StringBuilder();
        Random random = new Random();
        String mobile01;//手机号前三位
        int temp;
        switch (op) {
            case 0:
                mobile01 = CHINA_MOBILE[random.nextInt(CHINA_MOBILE.length)];
                break;
            case 1:
                mobile01 = CHINA_UNICOM[random.nextInt(CHINA_UNICOM.length)];
                break;
            case 2:
                mobile01 = CHINA_TELECOME[random.nextInt(CHINA_TELECOME.length)];
                break;
            default:
                mobile01 = "op标志位有误！";
                break;
        }
        if (mobile01.length() > 3) {
            return mobile01;
        }
        sb.append(mobile01);
        //生成手机号后8位
        for (int i = 0; i < 8; i++) {
            temp = random.nextInt(10);
            sb.append(temp);
        }
        return sb.toString();
    }



    //邮箱后缀
    public static final String[] EMAIL_SUFFIX = {
         "@gmail.com","@yahoo.com","@msn.com","@hotmail.com","@aol.com","@ask.com","@live.com","@qq.com","@0355.net","@163.com","@163.net","@263.net","@3721.net","@yeah"
    };

    public static String getRandomEmail(){
        return getSaltString(getRandomNum(3,10))+EMAIL_SUFFIX[getRandomNum(0,EMAIL_SUFFIX.length)];
    }

    /**
     * 生成随机字符串
     * @param length
     * @return
     */
    public static String getSaltString(Integer length) {

        String SALTCHARS ="ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890";

        StringBuilder salt=new StringBuilder();

        Random rnd = new Random();

        while (salt.length() < length) { // length of the random string.

            int index = (int) (rnd.nextFloat() * SALTCHARS.length());

            salt.append(SALTCHARS.charAt(index));

        }

        String saltStr = salt.toString();

        return saltStr;

    }


//========================================================================================================================= 集合数组

    /**
     * 从多个元素或数组中取一个随机元素
     *
     * @param arr 数组
     * @return 随机元素
     */
    @SafeVarargs
    public static <T> T getRandomElement(T... arr) {//
        int rand = (int) (Math.random() * (arr.length));
        return arr[rand];
    }

    /**
     * 从多个元素或数组中取一个随机元素
     *
     * @param arr 数组
     * @return 随机元素
     */
    public static Object getRandomElement(List arr) {//
        int rand = (int) (Math.random() * (arr.size()));
        return arr.get(rand);
    }

    /**
     * 从集合中取一个随机元素
     *
     * @param collection 集合
     * @return 随机元素
     */
    public static <T> T getRandomCollection(Collection<T> collection) {
        List<T> list = new ArrayList<>();
        list.addAll(collection);
        int rand = (int) (Math.random() * (collection.size()));
        return list.get(rand);
    }
    //========================================================================================================================= 日期


    /**
     * 生成随即日期
     *
     * @param lower 最低日期
     * @param upper 最高日期
     * @return 随即日期
     */
    public static Date getRandomDate(Date lower, Date upper) {//随机日期
        long from = lower.getTime();
        long to = upper.getTime();
        long result = from + (long) (Math.random() * (to - from));
        return new Date(result);
    }

    /**
     * 生成随即日期
     *
     * @return 随即日期
     */
    public static Date getRandomDate() {//随机日期
        Calendar calendar = Calendar.getInstance();
        calendar.set(2018, 1, 1, 23, 59, 59);
        long from = calendar.getTimeInMillis();
        long to = System.currentTimeMillis();
        long result = from + (long) (Math.random() * (to - from));
        return new Date(result);
    }

    //========================================================================================================================= 数据

    public static String getRandomString() {
        String str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
        Random random = new Random();
        StringBuffer sb = new StringBuffer();
        int length = (int) (Math.random() * 8 + 3);
        for (int i = 0; i < length; i++) {
            int number = random.nextInt(62);
            sb.append(str.charAt(number));
        }
        return sb.toString();
    }


    public static String getRandomString(int length) {
        String str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
        Random random = new Random();
        StringBuffer sb = new StringBuffer();
        for (int i = 0; i < length; i++) {
            int number = random.nextInt(62);
            sb.append(str.charAt(number));
        }
        return sb.toString();
    }

    /**
     * 用随机数据填充对象
     //        SubmitBookingOrderRequest submitBookingOrderRequest = (SubmitBookingOrderRequest) RandomUtil.fillObjectWithRandomString(new SubmitBookingOrderRequest(),
     //                new HashMap<String, List<String>>() {{
     //                    put("Long", Arrays.asList("pickUpPortId", "HeavyCabinetPortId", "originPortId", "targetPortId"));
     //                    put("String", Arrays.asList("packForm", "countryNo", "transportServiceType", "otherRequirement"));
     //                }},
     //                new HashMap<String, List<?>>() {{
     //                    put("packForm", Arrays.asList("9", "2", "5"));
     //                    put("countryNo", Arrays.asList("86", "7", "351", "349"));
     //                    put("pickUpPortId", Arrays.asList(14, 15, 16, 17, 18, 19, 20));
     //                    put("HeavyCabinetPortId", Arrays.asList(14, 15, 16, 17, 18, 19, 20));
     //                    put("originPortId", Arrays.asList(61, 76, 80, 110, 134));
     //                    put("targetPortId", Arrays.asList(1166, 1167, 3332, 1171, 3595, 3602));
     //                    put("transportServiceType", Arrays.asList("0,1", "0", "1", ""));
     //                    put("otherRequirement", Arrays.asList("自动化测试运单1", "自动化测试运单2", "自动化测试运单3", "自动化测试运单4"));
     //                }});
     * @param object         源对象
     * @param typeStringMap  类型名--字段名数组
     * @param matchStringMap 字段名--取值数组
     * @return
     */
    public static Object fillObjectWithRandomString(Object object,
                                                    Map<String, List<String>> typeStringMap,
                                                    Map<String, List<?>> matchStringMap) {
        Random random = new Random();
        BeanWrapper beanWrapper = new BeanWrapperImpl(object);
        PropertyDescriptor[] propertyDescriptorList = beanWrapper.getPropertyDescriptors();
        //开始遍历属性描述器
        out:
        for (PropertyDescriptor propertyDescriptor : propertyDescriptorList) {
            String currentFieldName = propertyDescriptor.getName();
            Class<?> currentFieldClass = propertyDescriptor.getPropertyType();
            //自定义检测
            if (typeStringMap.containsKey("String") && String.class.equals(currentFieldClass)) {
                for (String typeKeyNameString : typeStringMap.get("String")) {
                    if (currentFieldName.toLowerCase().contains(typeKeyNameString.toLowerCase())) {
                        beanWrapper.setPropertyValue(currentFieldName, String.valueOf(getRandomElement(matchStringMap.get(typeKeyNameString))));
                        continue out;
                    }
                }
            } else if (typeStringMap.keySet().contains("Long") && Long.class.equals(currentFieldClass)) {
                for (String typeKeyNameLong : typeStringMap.get("Long")) {
                    if (currentFieldName.toLowerCase().contains(typeKeyNameLong.toLowerCase())) {
                        beanWrapper.setPropertyValue(currentFieldName, getRandomElement(matchStringMap.get(typeKeyNameLong)));
                        continue out;
                    }
                }
            }
            //String类型处理
            if (String.class.equals(currentFieldClass)) {
                //邮箱检测
                if (currentFieldName.toLowerCase().contains("email")) {
                    beanWrapper.setPropertyValue(currentFieldName, getRandomString(6) + "@qq.com");
                    //手机检测
                } else if (currentFieldName.toLowerCase().contains("phone")) {
                    beanWrapper.setPropertyValue(currentFieldName, String.valueOf(getRandomLong(13330000000L, 19800000000L)));
                    //姓名检测
                } else if (currentFieldName.toLowerCase().contains("name")) {
                    beanWrapper.setPropertyValue(currentFieldName, getRandomName());
                } else {
                    beanWrapper.setPropertyValue(currentFieldName, getRandomString());
                }
                //Long类型处理
            } else if (Long.class.equals(currentFieldClass)) {
                beanWrapper.setPropertyValue(currentFieldName, getRandomLong(1L, 600L));
                //时间戳类型处理
            } else if (Timestamp.class.equals(currentFieldClass)) {
                beanWrapper.setPropertyValue(currentFieldName, Timestamp.valueOf(LocalDateTime.now()));
                //float类型处理
            } else if (float.class.equals(currentFieldClass) || double.class.equals(currentFieldClass)) {
                beanWrapper.setPropertyValue(currentFieldName, getRandomDouble(1, 600));
                //枚举
            } else if (currentFieldClass.isEnum()) {
                beanWrapper.setPropertyValue(currentFieldName, getRandomElement(currentFieldClass.getEnumConstants()));
                //Boolean
            } else if (Boolean.class.equals(currentFieldClass)) {
                beanWrapper.setPropertyValue(currentFieldName, true);
                //List 递归
            } else if (currentFieldClass.isAssignableFrom(List.class)) {
                Type currentRealType = null;
                try {
                    currentRealType = object.getClass().getDeclaredField(currentFieldName).getGenericType();
                } catch (NoSuchFieldException e) {
                    try {
                        currentRealType = object.getClass().getSuperclass().getDeclaredField(currentFieldName).getGenericType();
                    } catch (NoSuchFieldException e1) {
                        e1.printStackTrace();
                    }
                }

                if (currentRealType instanceof ParameterizedType) {
                    ParameterizedType parameterizedType = (ParameterizedType) currentRealType;
                    Class<?> tType = (Class<?>) parameterizedType.getActualTypeArguments()[0];
                    //创建泛型实例
                    int typeInstanceLength = random.nextInt(9) + 1;
                    List<Object> resultList = new ArrayList<>();
                    for (int i = 0; i < typeInstanceLength; i++) {
                        try {
                            resultList.add(fillObjectWithRandomString(tType.newInstance(), typeStringMap, matchStringMap));
                        } catch (InstantiationException | IllegalAccessException e) {
                            e.printStackTrace();
                        }
                    }
                    beanWrapper.setPropertyValue(currentFieldName, resultList);
                }

            } else if (Object.class.isAssignableFrom(currentFieldClass) && !"bytes".equals(currentFieldName) && !"class".equals(currentFieldName)) {
                try {
                    beanWrapper.setPropertyValue(currentFieldName, fillObjectWithRandomString(currentFieldClass.newInstance(), typeStringMap, matchStringMap));
                } catch (IllegalAccessException | InstantiationException exp) {
                    exp.printStackTrace();
                }
            }
        }
//        System.out.println("最终实例：" + beanWrapper.getWrappedInstance());
        return beanWrapper.getWrappedInstance();
    }


    /**
     * 取一个随机数
     *
     * @param lower 最低值
     * @param upper 最高值
     * @return int随机数
     */
    public static int getRandomInt(int lower, int upper) {
        int rand = (int) (Math.random() * (upper - lower)) + lower;
        return rand;
    }

    /**
     * 取一个随机数
     *
     * @param lower 最低值
     * @param upper 最高值
     * @return int随机数
     */
    public static Long getRandomLong(Long lower, Long upper) {
        Double rand = Math.random() * (upper - lower) + lower;
        return rand.longValue();
    }

    /**
     * 取一个随机数
     *
     * @param lower 最低值
     * @param upper 最高值
     * @return double随机数
     */
    public static double getRandomDouble(double lower, double upper) {
        return Math.random() * (upper - lower) + lower;
    }


    /**
     * 随机个数 生成 随机中文文字
     *
     * @return [1, 10)个随机中文
     */
    public static String getRandomText() {
        int time = getRandomInt(1, 10);
        String text[] = {"我", "你", "他", "炸", "了", "有", "和", "吃", "都", "在", "炸", "及", "问", "有", "方", "哦", "的", "额", "去", "还", "将", "到", "过", "后", "如", "票", "过", "日", "好", "跑", "家", "等", "怕"};
        String end = "";
        for (int i = 0; i < time; i++) {
            end += getRandomElement(text);
        }
        return end;
    }


    /**
     * 指定个数 生成 随机中文文字
     *
     * @param time 文字个数
     * @return time个中文字
     */
    public static String getRandomText(int time) {

        String text[] = {"我", "你", "他", "炸", "了", "有", "和", "吃", "都", "在", "炸", "及", "问", "有", "方", "哦", "的", "额", "去", "还", "将", "到", "过", "后", "如", "票", "过", "日", "好", "跑", "家", "等", "怕"};
        String end = "";
        for (int i = 0; i < time; i++) {
            end += getRandomElement(text);
        }
        return end;
    }

}
