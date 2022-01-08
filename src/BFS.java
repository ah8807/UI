import java.awt.*;
import java.io.File;
import java.util.*;
import java.util.concurrent.TimeUnit;

class BFS {
    public static void main(String[] args) {
        try {
            StdDraw.setCanvasSize(700, 700 );
            StdDraw.enableDoubleBuffering();
            Scanner sc = new Scanner(new File("C:\\Users\\lojze\\OneDrive\\Documents\\faks\\2.letnik\\1.semester\\UI\\maze\\labyrinth_7.txt"));
            int [][] labirinth;
            String[] polja;
            labirinth=new int[11][11];
            int i= 0;
            int[] start=new int[0];
            int[] end=new int[0];
            ArrayList<int[]> tresures= new ArrayList<int[]>();
            int tone =0;
            while(sc.hasNextLine()){
                String line = sc.nextLine();
                polja= line.strip().split(",");
                if(tone==0) {
                    labirinth = new int[polja.length][polja.length];
                    tone=1;
                }
                for (int j = 0; j < polja.length; j++) {
                    labirinth[i][j]=Integer.parseInt(polja[j]);
                    if(labirinth[i][j]==-2)
                        start = new int[]{i,j};
                    if(labirinth[i][j]==-4)
                        end = new int[]{i,j};
                    if(labirinth[i][j]==-3)
                        tresures.add(new int[]{i,j});

                }
                i++;
            }
            drawOut(labirinth);
            ArrayList<int[]> resitev = BFS(labirinth.length,labirinth.length,labirinth,start,end,tresures);
            StdDraw.setPenColor(Color.GREEN);
            Collections.reverse(resitev);
            for(int[]e : resitev){
                StdDraw.filledSquare((double) e[1] / (labirinth.length - 1), 1- (double) e[0] / (labirinth.length - 1), (double) 1 / (labirinth.length - 1) * 0.5);
                StdDraw.pause(100);
                StdDraw.show();
            }
            System.out.println("dolzina poti: "+resitev.size());
        }catch(Exception e){
            System.out.println(e);
        }
    }
    public static void drawOut(int[][] field) {
        StdDraw.setFont(new Font(null, Font.PLAIN, (int) (16 - (double) (field.length / 8))));
        for (int i = 0; i < field.length; i++) {
            for (int j = 0; j < field[0].length; j++) {
                switch (field[i][j]) {
                    case -1:
                        StdDraw.setPenColor(Color.BLACK);
                        break;
                    case -2:
                        StdDraw.setPenColor(Color.RED);
                        break;
                    case -3:
                        StdDraw.setPenColor(Color.YELLOW);
                        break;
                    case -4:
                        StdDraw.setPenColor(Color.GREEN);
                        break;
                    default:
                        StdDraw.setPenColor(Color.WHITE);
                        break;
                }
                StdDraw.filledSquare((double) j / (field.length - 1), 1- (double) i / (field.length -1), (double) 1 / (field.length - 1) * 0.5);
                StdDraw.setPenColor(Color.BLACK);
                StdDraw.text((double) j / (field.length - 1), 1 - (double) i / (field.length - 1),
                        String.valueOf(field[i][j]));
            }
        }
        StdDraw.show();
    }
    static int[][] dir = {{-1, 0}, {1, 0}, {0, -1}, {0, 1}};

    public static <Queue> ArrayList<int[]> BFS(int n, int m, int[][] maze, int[] start, int[] end, ArrayList<int[]> tresures) throws InterruptedException {
        // initialize the queue and visited matrix
        java.util.Queue<int[]> Q = new LinkedList<>();
        Q.add(start);
        boolean[][] visited = new boolean[n][m];
        // initialize the parent array
        int[][][] parent = new int[n][m][];
        visited[start[0]][start[1]] = true;
        // in practice you might want to stop as soon as end is visited
        while(!Q.isEmpty()) {
            int[] u = Q.poll();
            // we are now processing node u
            for(int[] d : dir) {
                int i = u[0] + d[0];
                int j = u[1] + d[1];
                // visit the edge from u to (i, j)
                if(!visited[i][j] && maze[i][j] !=-1) {
                    TimeUnit.MILLISECONDS.sleep(10);
                    StdDraw.setPenColor(Color.YELLOW);
                    StdDraw.filledSquare((double) j / (maze.length - 1), 1- (double) i / (maze.length - 1), (double) 1 / (maze.length - 1) * 0.5);
                    StdDraw.show();
                    // node (i, j) has not yet been visited and is not a wall, add it
                    Q.add(new int[] {i, j});
                    visited[i][j] = true;
                    // set the parent of (i, j) to be u
                    parent[i][j] = u;
                }
            }
            if(tresureVisited(tresures,visited)) {
                break;
            }
            if(visited[end[0]][end[1]]&&tresures.isEmpty())
                break;
        }
        int[] curend = end;
        // check whether a path exists
        for(int[] e : tresures){
            if(visited[e[0]][e[1]]) {
                tresures.remove(e);
                curend = e;
                break;
            }
        }

        ArrayList<int[]> path = new ArrayList<>();
        ArrayList<int[]> tmp = new ArrayList<>();

        drawOut(maze);
        if(!tresures.isEmpty()){
            tmp= BFS(n,m,maze,curend,end,tresures);
        }else if(curend!=end){
            tmp= BFS(n,m,maze,curend,end,tresures);
        }
        for(int[]e:tmp){
            path.add(e);
        }
        // build the path by tracing back from t to s
        int[] cur = curend;
        // loop until we reach s (s is the only visited node with null parent)
        while(parent[cur[0]][cur[1]] != null) {
            path.add(cur);
            cur = parent[cur[0]][cur[1]];
        }
        // reverse and return the path
        return path;
    }

    private static boolean tresureVisited(ArrayList<int[]> tresures,boolean[][] visited) {
        for(int[] e : tresures){
            if(visited[e[0]][e[1]])
                return true;
        }
        return false;
    }
}