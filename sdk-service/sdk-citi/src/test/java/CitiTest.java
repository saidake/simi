import com.google.i18n.phonenumbers.NumberParseException;
import com.google.i18n.phonenumbers.PhoneNumberUtil;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.Getter;
import org.apache.commons.lang3.StringUtils;

public class MainTest {
    @Data
    private static class Person{
        String name;
        Integer age;
        Son son;
    }
    @Data
    private static class Son{
        String sonName;
        Integer sonAge;
    }

    @AllArgsConstructor
    @Getter
    private  enum ZZ{
        _3("GP-3"),
        _4("GP-4");
        private String value;
    }
    public static void main(String[] args) throws NumberParseException {
//        Person person = new Person();
//        Optional<Son> son = Optional.of(person).map(Person::getSon);
//        Optional<Son> person1 = Optional.of(person).flatMap(item -> {
//            System.out.println(item);
//            return Optional.of(item.getSon());
//        }).flatMap(item -> {
//                    System.out.println(item);
//                    return Optional.of(item);
//                });
//        Integer integer = Optional.of(person).map(Person::getSon).map(Son::getSonAge).orElse(null);
//        System.out.println(integer);
        System.out.println(ZZ._3.getValue().equals("GP-3"));
        PhoneNumberUtil instance = PhoneNumberUtil.getInstance();
        System.out.println(instance.parse("+86 18274166484","US"));
        System.out.println(StringUtils.join("a","bccc","dfdfd"));
    }
}