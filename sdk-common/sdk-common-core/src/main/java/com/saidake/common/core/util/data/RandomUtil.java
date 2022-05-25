package com.saidake.common.core.util.data;

import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class RandomUtil {



    public static String familyName1 = "赵钱孙李周吴郑王冯陈褚卫蒋沈韩杨朱秦尤许何吕施张孔曹严华金魏陶姜戚谢邹喻水云苏潘葛奚范彭郎鲁韦昌马苗凤花方俞任袁柳鲍史唐费岑薛雷贺倪汤滕殷罗毕郝邬安常乐于时傅卞齐康伍余元卜顾孟平"
            + "黄和穆萧尹姚邵湛汪祁毛禹狄米贝明臧计成戴宋茅庞熊纪舒屈项祝董粱杜阮席季麻强贾路娄危江童颜郭梅盛林刁钟徐邱骆高夏蔡田胡凌霍万柯卢莫房缪干解应宗丁宣邓郁单杭洪包诸左石崔吉"
            + "龚程邢滑裴陆荣翁荀羊甄家封芮储靳邴松井富乌焦巴弓牧隗山谷车侯伊宁仇祖武符刘景詹束龙叶幸司韶黎乔苍双闻莘劳逄姬冉宰桂牛寿通边燕冀尚农温庄晏瞿茹习鱼容向古戈终居衡步都耿满弘国文东殴沃曾关红游盖益桓公晋楚闫";
    public static String familyName2 = "欧阳太史端木上官司马东方独孤南宫万俟闻人夏侯诸葛尉迟公羊赫连澹台皇甫宗政濮阳公冶太叔申屠公孙慕容仲孙钟离长孙宇文司徒鲜于司空闾丘子车亓官司寇巫马公西颛孙壤驷公良漆雕乐正宰父谷梁拓跋夹谷轩辕令狐段干百里呼延东郭南门羊舌微生公户公玉公仪梁丘公仲公上公门公山公坚左丘公伯西门公祖第五公乘贯丘公皙南荣东里东宫仲长子书子桑即墨达奚褚师吴铭";
    public static String girlName = "秀娟英华慧巧美娜静淑惠珠翠雅芝玉萍红娥玲芬芳燕彩春菊兰凤洁梅琳素云莲真环雪荣爱妹霞香月莺媛艳瑞凡佳嘉琼勤珍贞莉桂娣叶璧璐娅琦晶妍茜秋珊莎锦黛青倩婷姣婉娴瑾颖露瑶怡婵雁蓓纨仪荷丹蓉眉君琴蕊薇菁梦岚苑婕馨瑗琰韵融园艺咏卿聪澜纯毓悦昭冰爽琬茗羽希宁欣飘育滢馥筠柔竹霭凝晓欢霄枫芸菲寒伊亚宜可姬舒影荔枝思丽";
    public static String boyName = "伟刚勇毅俊峰强军平保东文辉力明永健世广志义兴良海山仁波宁贵福生龙元全国胜学祥才发武新利清飞彬富顺信子杰涛昌成康星光天达安岩中茂进林有坚和彪博诚先敬震振壮会思群豪心邦承乐绍功松善厚庆磊民友裕河哲江超浩亮政谦亨奇固之轮翰朗伯宏言若鸣朋斌梁栋维启克伦翔旭鹏泽晨辰士以建家致树炎德行时泰盛雄琛钧冠策腾楠榕风航弘";

    /**
     * 功能：随机产生姓氏
     *
     * @return
     */
    public static String getRamdonFamilyName() {
        String str = "";
        int randNum = new Random().nextInt(2) + 1;
        int strLen = randNum == 1 ? familyName1.length() : familyName2.length();
        int index = new Random().nextInt(strLen);
        if (randNum == 1) {
            str = String.valueOf(familyName1.charAt(index));
        } else {
            str = (index & 1) == 0 ? familyName2.substring(index, index + 2) :
                    familyName2.substring(index - 1, index + 1);
        }
        return str;
    }


    public static String getRandomName() {
        String currentSex=getRandomSex();
//        System.out.println(currentSex);
        return getRamdonFamilyName()+getRandomOnlyNameBySex(currentSex);
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
}
