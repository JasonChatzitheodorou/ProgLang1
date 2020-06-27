//import java.util.*;
import java.io.FileNotFoundException;
import java.util.Scanner;
import java.io.File;

public class Vaccine {
    public static void main(String[] args) {
        Globals g = new Globals();
        try {
            File myObj = new File(args[0]);
            Scanner myReader = new Scanner(myObj);
            int N = Integer.parseInt(myReader.nextLine().replace("\n", ""));
            for(int i = 0; i < N; i++) {
              String rna = myReader.nextLine().replace("\n", "");
              do_vaccine(rna);
            }
            myReader.close();
          } catch (FileNotFoundException e) {
            System.out.println("An error occurred.");
            e.printStackTrace();
          }
    }
    public static String hash_function(Node state) {
        String hash_string = new String("");
        for(String x : state.right) {
            hash_string += x;
        }
        for(String x : state.bases.keySet()) {
            hash_string += String.valueOf(state.bases.get(x));
        }
        hash_string += String.valueOf(state.comp);
        hash_string += String.valueOf(state.rev);
        return hash_string;
    }
    public static void do_vaccine(String rna) {
        Globals.initialise(rna);
        Globals.first_state = new Node(Globals.first_right,Globals.left.size()-1,Globals.first_dict, (Boolean)false, (Boolean)false, "", 0, 0, 0, (Boolean)false);
        Globals.id_list.put(0,Globals.first_state);
        Globals.bfs_queue.addFirst(Globals.first_state);

        // Does BFS
        Node curr_state;
        while(!Globals.bfs_queue.isEmpty()) {
            curr_state =Globals.bfs_queue.removeLast();

            if(curr_state.level ==Globals.level) {
            Globals.level += 1;
            Globals.hash_map.clear();

                if(Globals.level ==Globals.left.size() + 1) {
                Globals.bfs_queue.addLast(curr_state);
                    break;
                }
            }

            for(String move :Globals.moves) {
                if(curr_state.check_move(move)) {
                Globals.incremental_id += 1;

                    Node next_state = curr_state.do_move(move);

                    String key = hash_function(next_state);

                    if(Globals.hash_map.containsKey(key)) {
                        Node value =Globals.id_list.get(Globals.hash_map.get(key));

                        if(next_state.length < value.length){
                            value.is_bad = true;
                        Globals.hash_map.put(key, next_state.id);
                        }
                        else {
                            next_state.is_bad = true;
                        }
                    }
                    else {
                    Globals.hash_map.put(key, next_state.id);
                    }

                Globals.id_list.put(Globals.incremental_id, next_state);
                Globals.parent.put(Globals.incremental_id, curr_state.id);

                Globals.bfs_queue.addFirst(next_state);
                }
            }
        }
        
        // Finds best solution
        curr_state =Globals.bfs_queue.removeLast();
        int best_vaccine_of_level = curr_state.id;
        while(!Globals.bfs_queue.isEmpty()) {
            if(curr_state.length <Globals.id_list.get(best_vaccine_of_level).length) {
                best_vaccine_of_level = curr_state.id;
            }
            curr_state =Globals.bfs_queue.removeLast();
        }

        // Backtracks
        String vaccine = new String("");
        int curr = best_vaccine_of_level;
        while(Globals.parent.get(curr) != null) {
            vaccine =Globals.id_list.get(curr).move + vaccine;
            curr =Globals.parent.get(curr);
        }

        System.out.println(vaccine);
    }
    

}