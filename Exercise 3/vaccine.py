from collections import deque
import copy
import sys

filename = sys.argv[1]
file = open(filename, "rt")
#out = open("out.txt", "a")
#out.truncate(0)
N = int(file.readline().strip('\n'))

# Calculates a string of at most 4 characters
# Gives 1024 positions max (not bad)
def hash_function(state):
    hash_string = ""
    for x in state.right:
        hash_string += x

    for x in state.bases:
        hash_string += str(state.bases[x])

    hash_string += str(state.comp)
    hash_string += str(state.rev)
    return hash_string

# right is the current state of the right stack
# pos is the position in the left stack we have reached
# bases is a 4 element dictionary where we save which bases have been inserted
# rev is a flag on whether we should insert to the left or the right of the deque
# comp is a flag for choosing left stack or the complement of the left stack
class Node:
    def __init__(self, right, pos, bases, rev, comp, vaccine, length, id, level, is_bad):
        self.right = right
        self.pos = pos
        self.bases = bases
        self.rev = rev
        self.comp = comp
        self.vaccine = vaccine
        self.length = length
        self.id = id
        self.level = level
        self.is_bad = is_bad
    # Returns true if the move is valid
    def check_move(self, move):
        # if self.pos < -len(left):
        # return False
        # Choose the correct left stack
        temp_comp = self.comp
        if "c" in move:
            temp_comp = not self.comp
        left_stack = comp_left if temp_comp else left
        # Choose on which side of right to add the base and do it
        temp_rev = self.rev
        if "r" in move:
            temp_rev = not self.rev
        # Calculate the top of the left stack
        position = self.pos
        # Find the top element of the left stack
        left_top = left_stack[position]
        # Calculate the top of the right stack
        if self.right:
            right_top = self.right[-1] if temp_rev else self.right[0]
        else:
            right_top = None
        # Is true only if left_top base exists in right stack
        top_exists = self.bases[left_top]
        if self.is_bad or (top_exists and right_top != left_top):
            return False
        else:
            return True
    # Does the move and returns a new Node
    def do_move(self, move):
        next_state = copy.deepcopy(self)
        # Give unique id to this Node
        next_state.id = incremental_id
        # Choose the correct left stack and fix the comp flag
        if "c" in move:
            next_state.comp = not next_state.comp
        left_stack = comp_left if next_state.comp else left
        left_top = left_stack[next_state.pos]
        # Choose on which side of right to add the base and do it
        if "r" in move:
            next_state.rev = not next_state.rev
        if len(next_state.right) <= 1:
            if next_state.rev:
                next_state.right.append(left_top)
            else:
                next_state.right.insert(0, left_top)
        else:
            if next_state.rev:
                next_state.right[-1] = left_top
            else:
                next_state.right[0] = left_top
        # Update dictionary that the left_top base now exists
        next_state.bases[left_top] = True
        # Decrease the position on the left stack
        next_state.pos -= 1
        # Add the move to the vaccine
        next_state.vaccine = move
        # Calculate the new length
        next_state.length += len(move)
        # Store the current level
        next_state.level = level
        return next_state

# valid moves
moves = ["cp", "crp", "p", "rp"]

# Turns RNA base to its complement
complement_dict = {
    'A': 'U',
    'U': 'A',
    'G': 'C',
    'C': 'G'
}

for i in range(N):
    rna = file.readline().strip('\n')

    # left stack and its complement are global
    left = list(rna)
    comp_left = [complement_dict[x] for x in left]

    # Unique id for every node, increases on every iteration
    incremental_id = 0

    # first state of bfs
    first_dict = {
        'A': False,
        'G': False,
        'C': False,
        'U': False
    }
    first_state = Node([], -1, first_dict, False, False, "", 0, 0, 0, False)

    # Create bfs queue and append the first first
    bfs_queue = deque()
    bfs_queue.appendleft(first_state)

    # List that takes id and returns Node
    id_list = []
    id_list.append(first_state)

    # Parent list for every Node, useful for backtracking
    parent = []
    parent.append(None)

    # Stores the level of the bfs we are at
    level = 0
    new_level_flag = True

    # Stores the id of the last Node in the tree that could backtrack us to the best
    # vaccine compared to the rest of the level
    best_vaccine_of_level = None

    # Hash map
    hash_map = {}

    # Does BFS
    while bfs_queue:
        # Dequeue
        curr_state = bfs_queue.pop()

        # If we have finished with previous bfs level, increment level variable
        # If we have reached final level break so that the whole level is kept in the queue
        if curr_state.level == level:
            level += 1

            # Delete hash_map after entering new level
            del hash_map
            hash_map = {}

            if level == len(left) + 1:
                bfs_queue.append(curr_state)
                break

        # Execute the 4 possible moves
        for move in moves:
            if curr_state.check_move(move):
                incremental_id += 1

                # Do the move
                next_state = curr_state.do_move(move)

                #Decide whether it belongs in queue
                key = hash_function(next_state)
                if key in hash_map:
                    value = id_list[hash_map[key]]
                    if next_state.length < value.length:
                        value.is_bad = True
                        hash_map[key] = next_state.id
                    else:
                        next_state.is_bad = True
                else:
                    hash_map[key] = next_state.id

                # Save it to parent and id_list
                id_list.append(next_state)
                parent.append(curr_state.id)

                # Push the next state into the queue
                bfs_queue.appendleft(next_state)


    # REDUNDANT I could just search in the dictionary#
    # Find best solution in final level
    # Update if length is less than
    # NEVER if it is equal, so that we keep the lexicographically least solution
    curr_state = bfs_queue.pop()
    best_vaccine_of_level = curr_state.id
    while bfs_queue:
        if curr_state.length < id_list[best_vaccine_of_level].length:
            best_vaccine_of_level = curr_state.id
        curr_state = bfs_queue.pop()

    # Βacktrack
    curr = best_vaccine_of_level
    vaccine = ""
    while parent[curr] != None:
        vaccine = id_list[curr].vaccine + vaccine
        curr = parent[curr]
    print(vaccine)
    #out.write(vaccine + '\n')