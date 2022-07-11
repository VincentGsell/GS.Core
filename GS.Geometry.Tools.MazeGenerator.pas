//https://rosettacode.org/wiki/Maze_generation#Delphi
unit GS.Geometry.Tools.MazeGenerator;

interface

uses System.SysUtils,
     System.Types,
     System.Generics.Collections,
     GS.Geometry.Mesh2D,
     GS.Geometry.Mesh2D.Tools;

type
  TMCell = record
    Visited  : Boolean;
    PassTop  : Boolean;
    PassLeft : Boolean;
  end;
  TMaze  = array of array of TMCell;
  TRoute = TStack<TPoint>;

Type
TGSMazeGenerator = class
private
  FMaze : TMaze;
  FS,FE : TPoint;
  fwidth  : Uint16;
  fheight : UInt16;
protected
  procedure ClearVisited;
  procedure PrepareMaze;
  function MazeToString: String;
public
  constructor Create(const generateOnCreate : boolean = true); reintroduce;

  procedure generate(const width : Uint16 = 30; height : Uint16= 20);
  procedure solve(_Start, _End : TPoint);

  function AsString : String;
  function Maze : TMaze;
end;

TGSMaze = class
  Class procedure MazeToShapeWall(const Maze : TMaze; var ResulMesh : TGSRawMesh2D);
end;


implementation

function TGSMazeGenerator.AsString: String;
begin
  result := MazeToString;
end;

procedure TGSMazeGenerator.ClearVisited;
var
  x, y: Integer;
begin
  for y := 0 to fheight - 1 do
    for x := 0 to fwidth - 1 do
      FMaze[x, y].Visited := False;
end;

procedure TGSMazeGenerator.PrepareMaze;
var
  Route    : TRoute;
  P : TPoint;
  d        : Integer;
  Pool     : array of TPoint; // Pool of directions to pick randomly from
begin
  SetLength(FMaze, fwidth, fheight);
  ClearVisited;
  P := Point(Random(fwidth), Random(fheight));
  Route := TStack<TPoint>.Create;
  try
    while True do
    begin
      repeat
        SetLength(Pool, 0);
        if (P.y > 0)         and not FMaze[P.x, P.y-1].Visited then Pool := Pool + [Point(0, -1)];
        if (P.x < fwidth-1)  and not FMaze[P.x+1, P.y].Visited then Pool := Pool + [Point(1,  0)];
        if (P.y < fheight-1) and not FMaze[P.x, P.y+1].Visited then Pool := Pool + [Point(0,  1)];
        if (P.x > 0)         and not FMaze[P.x-1, P.y].Visited then Pool := Pool + [Point(-1, 0)];

        if Length(Pool) = 0 then // no direction to draw from
        begin
          if Route.Count = 0 then Exit; // and we are back at start so this is the end
          P := Route.Pop;
        end;
      until Length(Pool) > 0;

      d := Random(Length(Pool));
      P.Offset(Pool[d]);

      FMaze[P.x, P.y].Visited := True;
      if Pool[d].y = -1 then FMaze[P.x, P.y+1].PassTop  := True; // comes from down to up ( ^ )
      if Pool[d].x =  1 then FMaze[P.x, P.y].PassLeft   := True; // comes from left to right ( --> )
      if Pool[d].y =  1 then FMaze[P.x, P.y].PassTop    := True; // comes from left to right ( v )
      if Pool[d].x = -1 then FMaze[P.x+1, P.y].PassLeft := True; // comes from right to left ( <-- )
      Route.Push(P);
    end;
  finally
    Route.Free;
  end;
end;

procedure TGSMazeGenerator.solve(_Start, _End: TPoint);
var
  Route    : TRoute;
  Position : TPoint;
  V        : TPoint; // delta vector
begin
  assert(fwidth>1);
  Assert(fheight>1);
  ClearVisited;
  Position := _Start;
  Route    := TStack<TPoint>.Create;
  with Position do
  try
    FMaze[x, y].Visited := True;
    repeat
      if (y > 0)         and not FMaze[x, y-1].Visited and FMaze[x, y].PassTop    then V := Point(0, -1) else
      if (x < fwidth-1)  and not FMaze[x+1, y].Visited and FMaze[x+1, y].PassLeft then V := Point(1,  0) else
      if (y < fheight-1) and not FMaze[x, y+1].Visited and FMaze[x, y+1].PassTop  then V := Point(0,  1) else
      if (x > 0)         and not FMaze[x-1, y].Visited and FMaze[x, y].PassLeft   then V := Point(-1, 0) else
      begin
        if Route.Count = 0 then Exit;  // we are back at start so no way found
        Position := Route.Pop;         // step back
        Continue;
      end;

      Route.Push(Position);            // save current position to route
      Offset(V);                       // move forward
      Maze[x, y].Visited := True;
    until Position = _End;                // solved

    ClearVisited;
    while Route.Count > 0 do           // Route to Maze
      with Route.Pop do
        FMaze[x, y].Visited := True;

  finally
    Route.Free;
  end;
end;


constructor TGSMazeGenerator.Create(const generateOnCreate : boolean);
begin
  inherited Create;
  fwidth := 0;
  fheight := 0;
  if generateOnCreate then
    generate;
end;

procedure TGSMazeGenerator.generate(const width: Uint16; height: Uint16);
begin
  assert(width>1);
  Assert(height>1);
  fwidth := width;
  fheight := height;
  PrepareMaze;
  ClearVisited;
end;

function TGSMazeGenerator.Maze: TMaze;
begin
  result := FMaze;
end;


function TGSMazeGenerator.MazeToString: String;
var
  x, y: Integer;
  v   : Char;
begin
  Result := '';
  for y := 0 to fheight - 1 do
  begin
    for x := 0 to fwidth - 1 do
      if FMaze[x, y].PassTop then Result := Result + '+'#32#32#32 else Result := Result + '+---';
    Result := Result + '+' + sLineBreak;
    for x := 0 to fwidth - 1 do
    begin
      Result := Result + '|'#32[Ord(FMaze[x, y].PassLeft) + 1] + #32 + v + #32;
    end;
    Result := Result + '|' + sLineBreak;
  end;
  for x := 0 to fwidth - 1 do Result := Result + '+---';
  Result := Result + '+' + sLineBreak;
end;

{
procedure Main;
var
  Maze: TMaze;
begin
  Randomize;
  PrepareMaze(Maze);
  ClearVisited(Maze);     // show no route
  Write(MazeToString(Maze, Point(-1, -1), Point(-1, -1)));
  ReadLn;
end;
}


{ TGSMaze }

class procedure TGSMaze.MazeToShapeWall(const Maze: TMaze;
  var ResulMesh: TGSRawMesh2D);
var x, y : integer;
    lw,lh : integer;
    ltemp : IGSRawMesh;
    lb : boolean;

    procedure generatetop;
    begin
      ltemp.SetUpRect(1,0.1);
      ltemp.Modify(x,y-0.5,0);
      ResulMesh.Merge(TGSRawMesh2D(ltemp));
    end;

    procedure generateleft;
    begin
      ltemp.SetUpRect(0.1,1);
      ltemp.Modify(x-0.5,y,0);
      ResulMesh.Merge(TGSRawMesh2D(ltemp));
    end;

    procedure generatemark;
    begin
      ltemp.SetUpQuad(0.15);
      ltemp.Modify(x,y,0);
      ResulMesh.Merge(TGSRawMesh2D(ltemp));
    end;



begin
  Assert(length(Maze)>0);
  Assert(length(Maze[0])>0);
  Assert(Assigned(ResulMesh));
  ResulMesh.reset;

  ltemp := TGSRawMesh2D.Create;

  lw := length(Maze);
  lh := length(Maze[0]);

  for y := 0 to lh-1 do
  begin
    for x := 0 to lw-1 do begin
        if Not Maze[x, y].PassTop then
          generatetop;
    end;
    for x := 0 to lw-1 do  begin
        if Maze[x, y].Visited then
           generatemark;
        if Not Maze[x, y].PassLeft then
           generateleft;
    end;
    generateleft;
  end;

  for x := 0 to lw-1 do
    generatetop;

  x := 0;
  for y := 0 to lh-1 do
     generateleft;

  ResulMesh.reCenter;
end;

end.

{

Interesting C one...
/*
002	Michael Wen
003	6/1/2003
004	This program generates a random rectangular maze given the width and the height.
005	*/
006	#include<iostream>
007	#include<fstream>
008	#include<string>
009	#include<vector>
010	#include<ctime>
011	using namespace std;
012
013	double unit=1, xOffset, yOffset;
014	int width, height, numOfCells;
015	ofstream fout2;
016	vector<int> lottery, sol;
017
018	void remove(int);
019	void findSol(int, int);
020	void displaySol();
021
022	class Maze{
023	public:
024	    Maze(int);
025	    int find_root(int);
026	    void union_cell(int, int);
027
028	    vector<int> s;  /* parent's id */
029	    vector<double> xcoord;  /* lower right coordinate x */
030	    vector<double> ycoord;  /* lower right coordinate y */
031	/* for the next two vectors, -1 means down, 0 means must be up 1 means up */
032	    vector<int> down;   /* lower wall; knocked down or there */
033	    vector<int> right;  /* right wall; knocked down or there */
034	    vector<int> visited;  /* 1 means visited and 0 means not visited yet while finding solution */
035	};
036
037	Maze *d;
038
039	/*
040	precondition: n must be a positive integer
041	postcondition: s, xcoord, ycoord, down, right, visited are assigned values
042	*/
043	Maze::Maze(int n){
044	    int i,x,y,temp;
045
046	    for(i=0; i<n; i++){
047	        s.push_back(-1);
048	        x = i%width+1;
049	        y = height-i/width-1;
050	        xcoord.push_back(xOffset+x*unit);
051	        ycoord.push_back(yOffset+y*unit);
052	        temp = i>=width*(height-1) ? 0 : 1;
053	        down.push_back(temp);
054	        temp = (i+1)%width==0 ? 0 : 1;
055	        right.push_back(temp);
056	        visited.push_back(0);
057	    }
058	}
059	/*
060	precondition: n must be >= 0 and < s.size()
061	postcondition: return the root of n
062	*/
063	int Maze::find_root(int n){
064	    return s[n]<0 ? n : find_root(s[n]);
065	}
066	/*
067	precondition: root1 and root2 both must be >= 0 and < s.size()
068	postcondition: root1 and root2 belong to the same set
069	*/
070	void Maze::union_cell(int root1, int root2){
071	    s[find_root(root2)] = root1;
072	}
073
074	int main(int argc, char** argv){
075	/* exit if something is missing in the command line */
076	    if(argc!=5){
077	        cout << 'usage: exe <width> <height> <file1> <file2>
078	';
079	        cout << '<width>: # of columns, must be >= 2
080	';
081	        cout << '<height>: # of rows, must be >= 2
082	';
083	        cout << '<file1>: maze w/t solution file's name
084	';
085	        cout << '<file2>: maze w/ solution file's name
086	';
087	        cout << endl;
088	        exit(1);
089	    }
090
091	    char *file, *file2;
092	    ofstream fout;
093	    int i, victim, neighbor, neighbor2;
094	    width = atoi(argv[1]);
095	    height = atoi(argv[2]);
096	/* exit if the user provides unacceptable information */
097	    if(width<2 || height<2){
098	        cout << 'unacceptable command line, forced exit
099
100	';
101	        exit(1);
102	    }
103
104	/* initialize the random number generator */
105	    srand(time(0));
106
107	/* store critical data in variables */
108	    xOffset = width/20.0;
109	    yOffset = height/20.0;
110	    numOfCells = width*height;
111	    file = argv[3];
112	    file2 = argv[4];
113	    d = new Maze(width*height);
114
115	/* push all elements into a vector except the last one because it has no walls to knock down */
116	    for(i=0; i<width*height-1; i++)
117	        lottery.push_back(i);
118	/* use a while loop to construct the maze */
119	    while(lottery.size()!=0){
120	        victim = lottery[rand()%lottery.size()];
121	/*victim has two neighbors*/
122	        if(d->down[victim]!=0 && d->right[victim]!=0){
123	            neighbor = victim+1;
124	            neighbor2 = victim+width;
125	/* if neither of them is joined, pick one and knock down the mutual wall */
126	            if(d->find_root(neighbor)!=d->find_root(victim) &&
127	                d->find_root(neighbor2)!=d->find_root(victim)){
128	                if(rand()%2==0){
129	                    d->union_cell(victim, neighbor);
130	                    d->right[victim] = -1;
131	                }
132	                else{
133	                    d->union_cell(victim, neighbor2);
134	                    d->down[victim] = -1;
135	                }
136	            }
137	/* if only one of them is joined, join another one and knock down the intersecting wall AND remove victim in vector lottery */
138	            else if(d->find_root(neighbor)!=d->find_root(victim)){
139	                d->union_cell(victim, neighbor);
140	                d->right[victim] = -1;
141	                remove(victim);
142	            }
143	            else if(d->find_root(neighbor2)!=d->find_root(victim)){
144	                d->union_cell(victim, neighbor2);
145	                d->down[victim] = -1;
146	                remove(victim);
147	            }
148	/* if both of them are joined, remove victim in vector lottery */
149	            else
150	                remove(victim);
151
152	        }
153	/*victim has one neighbor*/
154	        else{
155	/* determine which neighbor it is and if they are joined if not joined, join them and knock down the wall if joined, do nothing */
156	            if((victim+1)%width==0){
157	                neighbor = victim+width;
158	                if(d->find_root(neighbor)!=d->find_root(victim)){
159	                    d->union_cell(victim, neighbor);
160	                    d->down[victim] = -1;
161	                }
162	            }
163	            else{
164	                neighbor=victim+1;
165	                if(d->find_root(neighbor)!=d->find_root(victim)){
166	                    d->union_cell(victim, neighbor);
167	                    d->right[victim] = -1;
168	                }
169	            }
170	            remove(victim);
171	        }
172	    }
173
174	/* generate code for matlab to display the maze */
175	    fout.open(file);
176	    fout2.open(file2);
177
178	/*first draw lines for outside walls in both files, note there are 4 openings*/
179	    fout << 'axis([0 ' << 2*xOffset+width*unit << ' 0 ' << 2*yOffset+height*unit        << ']);';
180	fout << endl;
181	/*upper line*/
182	    fout << 'x=[' << xOffset << ' ' << xOffset+width*unit << '];' << endl;
183	    fout << 'y=[' << yOffset+height*unit << ' ' << yOffset+height*unit << '];' <<           endl;
184	    fout << 'line(x,y)' << endl;
185	/*left line*/
186	    fout << 'x=[' << xOffset << ' ' << xOffset << '];' << endl;
187	    fout << 'y=[' << yOffset+height*unit-unit << ' ' << yOffset << '];' << endl;
188	    fout << 'line(x,y)' << endl;
189	/*right line*/
190	    fout << 'x=[' << xOffset+width*unit << ' ' << xOffset+width*unit << '];' <<             endl;
191	    fout << 'y=[' << yOffset+height*unit << ' ' << yOffset+unit << '];' << endl;
192	    fout << 'line(x,y)' << endl;
193	/*lower line*/
194	    fout << 'x=[' << xOffset << ' ' << xOffset+width*unit << '];' << endl;
195	    fout << 'y=[' << yOffset << ' ' << yOffset << '];' << endl;
196	    fout << 'line(x,y)' << endl;
197
198	/*second file...*/
199	    fout2 << 'axis([0 ' << 2*xOffset+width*unit << ' 0 ' <<                     2*yOffset+height*unit << ']);';
200	fout2 << endl;
201	/*upper line*/
202	    fout2 << 'x=[' << xOffset << ' ' << xOffset+width*unit << '];' << endl;
203	    fout2 << 'y=[' << yOffset+height*unit << ' ' << yOffset+height*unit << '];'           << endl;
204	    fout2 << 'line(x,y)' << endl;
205	/*left line*/
206	    fout2 << 'x=[' << xOffset << ' ' << xOffset << '];' << endl;
207	    fout2 << 'y=[' << yOffset+height*unit-unit << ' ' << yOffset << '];' << endl;
208	    fout2 << 'line(x,y)' << endl;
209	/*right line*/
210	    fout2 << 'x=[' << xOffset+width*unit << ' ' << xOffset+width*unit << '];' <<            endl;
211	    fout2 << 'y=[' << yOffset+height*unit << ' ' << yOffset+unit << '];' << endl;
212	    fout2 << 'line(x,y)' << endl;
213	/*lower line*/
214	    fout2 << 'x=[' << xOffset << ' ' << xOffset+width*unit << '];' << endl;
215	    fout2 << 'y=[' << yOffset << ' ' << yOffset << '];' << endl;
216	    fout2 << 'line(x,y)' << endl;
217
218	/*draw interior walls in both files*/
219	    for(i=0; i<numOfCells; i++){
220	        if(d->right[i]==1 && d->down[i]==1){
221	            fout << 'x=[' << d->xcoord[i]-unit << ' ' << d->xcoord[i] << ' ';
222	fout << d->xcoord[i] << '];' << endl;
223	            fout << 'y=[' << d->ycoord[i] << ' ' << d->ycoord[i] << ' ';
224	fout << d->ycoord[i]+unit << '];' << endl;
225	            fout << 'line(x,y)' << endl;
226
227	            fout2 << 'x=[' << d->xcoord[i]-unit << ' ' << d->xcoord[i] << ' ';
228	fout2 << d->xcoord[i] << '];' << endl;
229	            fout2 << 'y=[' << d->ycoord[i] << ' ' << d->ycoord[i] << ' ';
230	fout2 << d->ycoord[i]+unit << '];' << endl;
231	            fout2 << 'line(x,y)' << endl;
232	        }
233	        else if(d->right[i]==1){
234	            fout << 'x=[' << d->xcoord[i] << ' ' << d->xcoord[i] << '];' <<                   endl;
235	            fout << 'y=[' << d->ycoord[i] << ' ' << d->ycoord[i]+unit << '];'                   << endl;
236	            fout << 'line(x,y)' << endl;
237	            fout2 << 'x=[' << d->xcoord[i] << ' ' << d->xcoord[i] << '];' <<                  endl;
238	            fout2 << 'y=[' << d->ycoord[i] << ' ' << d->ycoord[i]+unit <<                   '];' << endl;
239	            fout2 << 'line(x,y)' << endl;
240	        }
241	        else if(d->down[i]==1){
242	            fout << 'x=[' << d->xcoord[i]-unit << ' ' << d->xcoord[i] << '];'                   << endl;
243	fout << 'y=[' << d->ycoord[i] << ' ' << d->ycoord[i] << '];' <<           endl;
244	            fout << 'line(x,y)' << endl;
245	            fout2 << 'x=[' << d->xcoord[i]-unit << ' ' << d->xcoord[i] <<                       '];' << endl;
246	            fout2 << 'y=[' << d->ycoord[i] << ' ' << d->ycoord[i] << '];' <<                  endl;
247	            fout2 << 'line(x,y)' << endl;
248	        }
249	    }
250	    fout.close();
251
252	/* find and put solution code in the solution file */
253	    findSol(0, numOfCells-1);
254
255	    return 0;
256	}
257
258	/*
259	precondition: victim should, but not must, be an element in lottery
260	postcondition: victim is erased from lottery
261	*/
262	void remove(int victim){
263	    vector<int>::iterator vi;
264	    for(vi=lottery.begin(); vi!=lottery.end(); vi++)
265	        if(*vi==victim){
266	            lottery.erase(vi);
267	            return;
268	        }
269	}
270	/*
271	precondition: from and to must be >= 0 and < numOfCells
272	postcondition: findSol terminates by either calling displaySol or running out; sol stores the solution to the maze
273	*/
274	void findSol(int from, int to){
275	    sol.push_back(from);
276	    d->visited[from] = 1;
277	    if(from==to){
278	        displaySol();
279	        exit(0);
280	    }
281	/*right wall is knocked down*/
282	    if(from>=0 && from<numOfCells && d->right[from]==-1){
283	        if(d->visited[from+1]!=1) {
284	            findSol(from+1, to);
285	            sol.pop_back();
286	        }
287	    }
288	/*lower wall is knocked down*/
289	    if(from>=0 && from<numOfCells && d->down[from]==-1){
290	        if(d->visited[from+width]!=1) {
291	            findSol(from+width, to);
292	            sol.pop_back();
293	        }
294	    }
295	/*upper wall is knocked down*/
296	    if(from-width>=1 && from-width<numOfCells &&
297	d->down[from-width]==-1){
298	        if(d->visited[from-width]!=1) {
299	            findSol(from-width, to);
300	            sol.pop_back();
301	        }
302	    }
303	/*left wall is knocked down*/
304	    if(from%width!=0 && from-1>=1 && from-1<numOfCells &&
305	d->right[from-1]==-1){
306	        if(d->visited[from-1]!=1) {
307	            findSol(from-1, to);
308	            sol.pop_back();
309	        }
310	    }
311	}
312	/*
313	precondition: none
314	postcondition: put code for displaying the maze in file2
315	*/
316	void displaySol(){
317	    int i;
318
319	    if(sol.size()<=1){
320	        fout2.close();
321	        return;
322	    }
323	    fout2 << 'hold on
324	';
325	/* construct vectors x and y then use plot command to plot 'g' in plot means green */
326	    fout2 << 'x=[';
327	    for(i=0; i<sol.size(); i++){
328	        fout2 << d->xcoord[sol[i]]-unit/2  <<  ' ';
329	    }
330	    fout2 << '];
331	y=[';
332	    for(i=0; i<sol.size(); i++){
333	        fout2 << d->ycoord[sol[i]]+unit/2  <<  ' ';
334	    }
335	    fout2 << '];' << endl;
336	    fout2 << 'plot(x,y,'g')' << endl;
337
338	    fout2.close();
339	}
}