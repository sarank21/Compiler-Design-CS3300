
import java.util.*;

public class Tests {
    public static void main(String args[]) {
        HashMap<String, String> map = new HashMap<>();
        map.put("A1", "a1");
        map.put("A2", "a5");
        map.put("A3", "a5");
        map.put("A4", "a5");
        map.put("A5", "a5");
        for (Map.Entry<String,String> f : map.entrySet())
        {
            System.out.println(f.getKey()+" "+f.getValue());
        }
    }
}
