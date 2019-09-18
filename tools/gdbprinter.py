import gdb.printing

class InstPrinter(object):
    def __init__(self, val):
        self.val = val

    def topsort_dfs(self, root, visited, stack):
        visited[root] = True
        start = root['Ops']['_M_impl']['_M_start']
        finish = root['Ops']['_M_impl']['_M_finish']

        item = start
        while item != finish:
            if not item in visited:
                self.topsort_dfs(item, visited, stack)
            item += 1

        stack.append(root)

    def get_nr(self, instptr):
        instrnr = int(instptr)
        if instrnr in self.printed:
            return self.printed[instrnr]
        else:
            res = self.counter
            self.printed[instrnr] = res
            self.counter += 1
            return res

    def print_node(self, node):
        start = node['Ops']['_M_impl']['_M_start']
        end = node['Ops']['_M_impl']['_M_finish']

        if not int(node) in self.printed:
            nr = self.get_nr(node)
            ikind = str(node['K']).split('::')[-1].lower()
            res = '%%%d:i%d = %s ' % (nr, int(node['Width']), ikind)
            while start != end:
                res += '%%%d, ' % (self.get_nr(start))
                start += 1

            res = res.strip(', ')
            print (res)

    def to_string(self):
        print ('---')
        self.printed = dict()
        self.counter = 0
        visited = dict()
        stack = list()
        self.topsort_dfs(self.val.address, visited, stack)

        for it in stack:
            self.print_node(it)

        return '---'

def lookup_type(val):
    if str(val.type) == 'souper::Inst':
        return InstPrinter(val)
    return None

gdb.pretty_printers.append(lookup_type)
