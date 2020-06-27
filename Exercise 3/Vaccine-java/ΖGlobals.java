import java.util.*;

public class Globals {
    public static ArrayList<String> moves;
    public static HashMap<String, String> complement_dict;
    public static String rna;
    public static ArrayList<String> left;
    public static ArrayList<String> comp_left;
    public static int incremental_id;
    public static HashMap<String, Boolean> first_dict;
    public static ArrayList<String> first_right;
    public static Node first_state;
    public static Deque<Node> bfs_queue;    
    public static HashMap<Integer, Node> id_list;
    public static HashMap<Integer, Integer> parent;
    public static HashMap<String, Integer> hash_map;
    public static int level;

    Globals() {;}

    public static void initialise(String rna) {
        moves = new ArrayList<String>(Arrays.asList("cp", "crp", "p", "rp"));
        first_dict = new HashMap<>();
        first_dict.put("A", false);
        first_dict.put("U", false);
        first_dict.put("G", false);
        first_dict.put("C", false);

        first_right = new ArrayList<String>();
        bfs_queue = new LinkedList<Node>();
        id_list = new HashMap<>();
        parent = new HashMap<>();
        hash_map = new HashMap<>();

        complement_dict = new HashMap<>();
        complement_dict.put("U", "A");
        complement_dict.put("A", "U");
        complement_dict.put("G", "C");
        complement_dict.put("C", "G");

        Globals.rna = rna;
        Globals.left = new ArrayList<>(Arrays.asList(Globals.rna.split("")));
        Globals.comp_left = new ArrayList<>();

        for(String x : Globals.left){
            Globals.comp_left.add(Globals.complement_dict.get(x));
        } 
        Globals.incremental_id = 0;
        Globals.level = 0;
        
        Globals.parent.put(0, null);
    }

    public void clear() {
        Globals.comp_left.clear();
        Globals.id_list.clear();
        Globals.bfs_queue.clear();
        Globals.hash_map.clear();
    }
}