# Name:
#   Frankie James
# UID:
#   919840157
# Others With Whom I Discussed Things:
#
# Other Resources I Consulted:
#
#

import sys

def rowStringToDict(headers, ln):
    result = {}
    line_list = ln.strip().split(",")
    for index, title in enumerate(headers):
        result[title] = line_list[index]
    return result

assert(rowStringToDict(['Name','Age','Hair'], 'Steve,25,Blonde') == {'Name' : 'Steve', 'Age' : '25', 'Hair' : 'Blonde'})

def rowDictToString(headers, entry):
    result = []
    for title in headers:
        # if title in entry:
            result.append(str(entry[title]))
    return ",".join(result)

assert(rowDictToString(['Name','Age','Hair'], {'Name' : 'Steve', 'Age' : '25', 'Hair' : 'Blonde'}) == 'Steve,25,Blonde')

class Identity:
    
    def __init__(self, in_headers, args):
        self.input_headers = list(in_headers)
        self.output_headers = list(in_headers)
        self.aggregate_headers = []

    def process_row(self,row):
        # Do nothing; return the row unchanged.
        return row
    
    def get_aggregate(self):
        # No aggregation, return an empty row.
        return {}

class Count:
    
    def __init__(self, in_headers, args):
        self.input_headers = list(in_headers)
        self.output_headers = list(in_headers)
        self.aggregate_headers = ['Count']

        # state for the aggregation
        self.count = 0

    def process_row(self,row):
        # update the state
        self.count += 1

        # return the row unchanged
        return row

    def get_aggregate(self):
        # return the aggregate row 
        return {'Count' : self.count}

def runQuery(f, query):
    print(",".join(query.output_headers))

    for line in f:
        dict_line = rowStringToDict(query.input_headers, line)
        output_row = query.process_row(dict_line)
        if output_row is not None:
            print(rowDictToString(query.output_headers, output_row))

    if len(query.aggregate_headers) > 0:
        aggregate_row = []
        aggregate_result = query.get_aggregate()
        for title in query.aggregate_headers:
            aggregate_row.append(str(aggregate_result[title]))
        print("\n" + ",".join(query.aggregate_headers) + "\n" + ",".join(aggregate_row))

  
class Rename:
    
    def __init__(self, in_headers, args):
        if len(args) < 2:
            raise Exception("Rename expected two arguments: an old header name, and a new header name")
        self.old_header, self.new_header, self.input_headers, self.aggregate_headers = args.pop(0), args.pop(0), list(in_headers), []
        if self.old_header not in self.input_headers or self.new_header in self.input_headers:
            raise Exception("Rename expected the the old header to present in the input headers and for the new header not to be")
        self.output_headers = list(in_headers)
        self.output_headers[self.output_headers.index(self.old_header)] = self.new_header


    def process_row(self,row):
        row[self.new_header] = row[self.old_header]
        return row

    def get_aggregate(self):
        return {}

    
class Swap:
    
    def __init__(self, in_headers, args):
        if len(args) < 2:
            raise Exception("Swap expected two arguments: an old header name, and a new header name")
        self.first_header, self.second_header, self.input_headers, self.aggregate_headers = args.pop(0), args.pop(0), list(in_headers), []
        self.output_headers = list(in_headers)
        header_one_index, header_two_index = self.output_headers.index(self.first_header), self.output_headers.index(self.second_header)
        self.output_headers[header_one_index], self.output_headers[header_two_index] = self.output_headers[header_two_index], self.output_headers[header_one_index]   


    def process_row(self,row):
        return row

    def get_aggregate(self):
        return {}

class Select:
    def __init__(self, in_headers, args):
        if len(args) < 1:
            raise Exception("Select expected at least one column name to output")
        self.input_headers, self.output_headers, self.aggregate_headers = list(in_headers), [], []
        flag_triggered = False
        while len(args) > 0 and not flag_triggered:
            item = args[0]
            if item[0] == "-": flag_triggered = True
            if not flag_triggered and item in in_headers:
                self.output_headers.append(args.pop(0))

    def process_row(self, row):
        return row

    def get_aggregate(self):
        return {}

class Filter:

    def __init__(self, in_headers, args):
        if len(args) < 1:
            raise Exception("Filter expected at least argument as a predicate")
        self.input_headers, self.output_headers, self.aggregate_headers, self.predicate = list(in_headers), list(in_headers), [], args.pop(0)


    def process_row(self,row):
        return row if eval(self.predicate, row) else None

    def get_aggregate(self):
        return {}
    

class Update:
    
    def __init__(self, in_headers, args):
        if len(args) < 2:
            raise Exception("Update expected a column to be updated with a predicate (Two arguments)")
        self.input_headers, self.output_headers, self.aggregate_headers, self.col_of_interest, self.predicate = list(in_headers), list(in_headers), [], args.pop(0), args.pop(0)
        if self.col_of_interest not in self.input_headers:
            raise Exception("Update was provided a column to change, not in the given headers")


    def process_row(self,row):
        row[self.col_of_interest] = eval(self.predicate, row)
        return row

    def get_aggregate(self):
        return {}

class Add:

    def __init__(self, in_headers, args):
        if len(args) < 2:
            raise Exception("Update expected a column to be updated with a predicate (Two arguments)")
        self.input_headers, self.output_headers, self.aggregate_headers, self.new_column, self.predicate = list(in_headers), list(in_headers), [], args.pop(0), args.pop(0)
        if self.new_column in self.input_headers:
            raise Exception("Add was provided a column to change, but the column is already present")
        self.output_headers.append(self.new_column)
               
    def process_row(self,row):
        row[self.new_column] = eval(self.predicate, row)
        return row

    def get_aggregate(self):
        return {}

class MaxBy:
   
    def __init__(self, in_headers, args):
        if len(args) < 2:
            raise Exception("MaxBy expected two columns as arguments")
        self.input_headers, self.output_headers, self.title_column, self.max_column = list(in_headers), list(in_headers), args.pop(0), args.pop(0)
        self.aggregate_identifier = "Max " + str(self.title_column) + " By " + str(self.max_column)
        self.aggregate_headers, self.max_aggregate = [self.aggregate_identifier], None
        if self.title_column not in self.input_headers or self.max_column not in self.input_headers:
            raise Exception("MaxBy recieved columns not present in the given table")
    def process_row(self,row):
        self.max_aggregate = (row[self.title_column], int(row[self.max_column])) if (self.max_aggregate == None or int(row[self.max_column]) > self.max_aggregate[1]) else self.max_aggregate
        return row
    
    def get_aggregate(self):
        return {self.aggregate_identifier: self.max_aggregate[0]}

class Sum:
    
    def __init__(self, in_headers, args):
        if len(args) < 1:
            raise Exception("Sum expected a column as an argument to sum")
        self.input_headers, self.output_headers, self.sum_column = list(in_headers), list(in_headers), args.pop(0)
        if self.sum_column not in self.input_headers:
            raise Exception("The column provided to Sum is not in the input")
        self.aggregate_identifier = str(self.sum_column) + " Sum"
        self.aggregate_headers = [self.aggregate_identifier]

        self.sum = 0

    def process_row(self,row):
        self.sum += int(row[self.sum_column])
        return row

    def get_aggregate(self):
        return {self.aggregate_identifier: self.sum}

class Mean:
    
    def __init__(self, in_headers, args):
        if len(args) < 1:
            raise Exception("Mean expected a column as an argument to sum")
        self.input_headers, self.output_headers, self.mean_column = list(in_headers), list(in_headers), args.pop(0)
        if self.mean_column not in self.input_headers:
            raise Exception("The column provided to Mean is not in the input")
        self.aggregate_identifier = str(self.mean_column) + " Mean"
        self.aggregate_headers = [self.aggregate_identifier]

        self.sum_count = dict(sum = 0, count = 0)

    def process_row(self,row):
        self.sum_count['sum'], self.sum_count['count'] = self.sum_count['sum'] + int(row[self.mean_column]), self.sum_count['count'] + 1
        return row

    def get_aggregate(self):
        return {self.aggregate_identifier: self.sum_count['sum'] / self.sum_count['count']}

    
class ComposeQueries:
    
    def __init__(self, q1, q2):
        if q1.output_headers != q2.input_headers:
            raise Exception("Compose recieved incompatible queries")
        self.q1, self.q2 = q1, q2
        self.input_headers = q1.input_headers
        self.output_headers = q2.output_headers
        self.aggregate_headers = q1.aggregate_headers + q2.aggregate_headers


    def process_row(self,row):
        result_1 = self.q1.process_row(row)
        return None if result_1 == None else self.q2.process_row(result_1)

    def get_aggregate(self):
        result = self.q1.get_aggregate().copy()
        result.update(self.q2.get_aggregate())
        return result

#################### Test it! ####################

def runComposite():
    f = open('player_career_short.csv')

    # get the input headers
    in_headers = f.readline().strip().split(',')

    # build the query
    args = ['-Add', 'stealsPerGame', 'int(stl)/int(gp)', '-MaxBy', 'id', 'stealsPerGame']

    # ok, first query is Add. pop off the flag.
    args.pop(0)

    # build the first query
    q1 = Add(in_headers, args)

    # the input of second query is the output of first query
    next_in_headers = q1.output_headers

    assert(args == ['-MaxBy', 'id', 'stealsPerGame'])

    # ok, second query is MaxBy. pop off the flag.
    args.pop(0)
    
    q2 = MaxBy(next_in_headers, args)

    assert(args == [])

    # build the composite query.
    query = ComposeQueries(q1,q2)
    
    # run it.
    runQuery(f, query)


def runTester(testName):
    f = open('player_career_short.csv')
    in_headers = f.readline().strip().split(',')
    args = list(testDict[str(testName).split(".")[1][:-2]]["args"])
    assertion_args = testDict[str(testName).split(".")[1][:-2]]["assertter"]
    query = testName(in_headers, args)
    assert(args == assertion_args)
    runQuery(f, query)

testDict = {
    'Identity' : dict(args=[], assertter=[]),
    'Count'    : dict(args=[], assertter=[]),
    'Rename'   : dict(args=['gp','GamesPlayed'], assertter=[]),
    'Select'   : dict(args=['firstname','lastname', 'gp', 'pts', '-Stop'], assertter=['-Stop']),
    'Swap'     : dict(args=['firstname','lastname', 'NotUsed!'], assertter=['NotUsed!']),
    'Filter'   : dict(args=['int(gp) > 500', "Something"], assertter=["Something"]),
    'Update'   : dict(args=['firstname', 'firstname.upper()', "Something", "Else"], assertter=["Something", "Else"]),
    'Add'      : dict(args=["Frankie's Column", "''.join(list(id)[::-1])", "Something", "Else"], assertter=["Something", "Else"]),
    'MaxBy'    : dict(args=["id", "minutes", "Something", "Else"], assertter=["Something", "Else"]),
    'Sum'      : dict(args=["turnover", "Something", "Else"], assertter=["Something", "Else"]),
    'Mean'     : dict(args=["turnover", "Something", "Else"], assertter=["Something", "Else"])
    }

queries = {
    'Identity' : Identity,
    'Rename'   : Rename,
    'Select'   : Select,
    'Swap'     : Swap,
    'Filter'   : Filter,
    'Update'   : Update,
    'Add'      : Add,
    'Count'    : Count,
    'MaxBy'    : MaxBy,
    'Sum'      : Sum,
    'Mean'     : Mean
    }

def buildQuery(in_headers, args):
    
    query = Identity(in_headers,args)

    if args[0][0] != "-":
        raise Exception("The first argument given was not a query")

    while(len(args) > 0):
        new_query = args.pop(0)[1:]
        built_query = queries[new_query](query.output_headers, args)
        query = ComposeQueries(query, built_query)

    return query

# Example: Maximum number of points per minute. Filter out players that played 0 minutes to prevent divide by zero.
#
# $ python3 hw4.py player_career.csv -Filter "float(minutes) > 0" -Add ppm "float(pts)/float(minutes)" -MaxBy ppm ppm
#
# Example: Show the first and last names of the players with the maximum points per game, and the maximum number of points per minute.
#
# $ python3 hw4.py player_career.csv -Filter "float(minutes) > 0" -Add first_last "firstname + ' ' + lastname" -Add ppg "float(pts)/float(gp)" -Add ppm "float(pts)/float(minutes)" -MaxBy first_last ppg -MaxBy first_last ppm
#
# Example: Now include maximum values of pointsPerGame and pointsPerMinute, and end with -Filter "False" to show only the aggregate row.
#
# $ python3 hw4.py player_career.csv -Filter "float(minutes) > 0" -Add first_last "firstname + ' ' + lastname" -Add ppg "float(pts)/float(gp)" -Add ppm "float(pts)/float(minutes)" -MaxBy first_last ppg -MaxBy ppg ppg -MaxBy first_last ppm -MaxBy ppm ppm -Filter "False"
#
# Example: Count the number of players with at least 1 point per minute on average.
#
# $ python3 hw4.py player_career.csv -Filter "int(minutes) > 0" -Add ppm "float(pts)/float(minutes)" -Filter "float(ppm) > 1" -Count

##############################################################

def main():   
    # arguments start from position 1, since position 0 is always 'hw4.py'
    args = sys.argv[1:]

    # first argument is the input file. see help(list.pop)
    fname = args.pop(0)
    
    # Open that file! see help(open)
    f = open(fname)

    in_headers = f.readline().strip().split(',')
    
    # build the query
    query = buildQuery(in_headers, args)

    # after building the query, all arguments should be consumed.
    assert(len(args) == 0)
    
    # run the query.
    runQuery(f,query)

if __name__ == "__main__":
    main()
    
