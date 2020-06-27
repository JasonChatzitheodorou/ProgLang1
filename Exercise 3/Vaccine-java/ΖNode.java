import java.util.*;

/**
 * Node
 */
public class Node {
    public ArrayList<String> right;
    public int pos;
    public HashMap<String, Boolean> bases;
    public Boolean rev;
    public Boolean comp;
    public String move;
    public int length;
    public int id;
    public int level;
    public Boolean is_bad;

    Node(ArrayList<String> right, int pos, HashMap<String, Boolean> bases, Boolean rev, Boolean comp, String move,
            int length, int id, int level, Boolean is_bad) {
        
        this.right = right;
        this.pos = pos;
        this.bases = bases;
        this.rev = rev;
        this.comp = comp;
        this.move = move;
        this.length = length;
        this.id = id;
        this.level = level;
        this.is_bad = is_bad;
    }

    Node(Node state) {

        this.right = new ArrayList<String>();
        this.right.addAll(state.right);
        this.pos = state.pos;
        this.bases = new HashMap<String, Boolean>();
        this.bases.putAll(state.bases);
        this.rev = state.rev;
        this.comp = state.comp;
        this.move = state.move;
        this.length = state.length;
        this.id = state.id;
        this.level = state.level;
        this.is_bad = state.is_bad;
    }

    public Boolean check_move(String move){
        Boolean temp_comp = move.contains("c") ? ! this.comp : this.comp;
        Boolean temp_rev = move.contains("r") ? ! this.rev : this.rev;
        ArrayList<String> left_stack = temp_comp ? Globals.comp_left : Globals.left;
        int position = this.pos;
        String left_top = left_stack.get(position);
        String right_top = new String();

        if(! this.right.isEmpty()) {
            right_top = temp_rev ? this.right.get(this.right.size() - 1) : this.right.get(0);
        }
        else {
            right_top = "";
        }
        
        Boolean top_exists = this.bases.get(left_top);

        if(this.is_bad || (top_exists && ! right_top.equals(left_top))){
            return (Boolean)false;
        }
        else {
            return (Boolean)true;
        }
    }

    public Node do_move(String move) {
        Node next_state = new Node(this);
        
        next_state.id = Globals.incremental_id;
        
        if(move.contains("c")) next_state.comp = ! next_state.comp;
        
        ArrayList<String> left_stack = next_state.comp ? Globals.comp_left : Globals.left;
        String left_top = left_stack.get(next_state.pos);

        if(move.contains("r")) next_state.rev = ! next_state.rev;

        if(next_state.right.size() <= 1){
            if(next_state.rev) next_state.right.add(left_top);
            else next_state.right.add(0, left_top);
        }
        else {
            if(next_state.rev) next_state.right.set(next_state.right.size() - 1, left_top);
            else next_state.right.set(0, left_top);
        }

        next_state.bases.put(left_top, true);
        next_state.pos -= 1;
        next_state.move = move;
        next_state.length += move.length();
        next_state.level = Globals.level;
        return next_state;
    }
}