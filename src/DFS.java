import java.awt.*;
import java.io.File;
import java.util.*;
import java.util.concurrent.TimeUnit;

public class DFS {
    static int path_length=0;
    static int cost=0;
    static int n_nodes_visited=0;
    static int greatest_depth=0;
    static int speed=100;

    public static void main(String[] args) {
        try {
            StdDraw.setCanvasSize(700, 700 );
            StdDraw.enableDoubleBuffering();
            Scanner sc = new Scanner(new File("C:\\Users\\lojze\\OneDrive\\Documents\\faks\\2.letnik\\1.semester\\UI\\maze\\labyrinth_1.txt"));
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
            System.out.println("DFS");
            drawOut(labirinth);
            ArrayList<int[]> resitev = DFS(labirinth.length,labirinth.length,labirinth,start,end,getTresures(tresures));
            Collections.reverse(resitev);
            drawSolution(labirinth, resitev);
            cost = resitev.size();
            path_length = resitev.size();
            printStatistics();
            resetStatistics();

            System.out.println("\nBFS");
            drawOut(labirinth);
            resitev = BFS(labirinth.length,labirinth.length,labirinth,start,end,getTresures(tresures));
            Collections.reverse(resitev);
            Collections.reverse(resitev);
            drawSolution(labirinth, resitev);
            cost = resitev.size();
            path_length = resitev.size();
            printStatistics();
            resetStatistics();

            System.out.println("\nAstar");
            drawOut(labirinth);
            resitev = AStar(labirinth.length,labirinth.length,labirinth,start,end,getTresures(tresures));
            Collections.reverse(resitev);
            Collections.reverse(resitev);
            drawSolution(labirinth, resitev);
            path_length = resitev.size();
            printStatistics();
            resetStatistics();


        }catch(Exception e){
            System.out.println(e);
        }
    }

    private static ArrayList<int[]> getTresures(ArrayList<int[]> tresures) {
        return (ArrayList)tresures.clone();
    }

    private static void resetStatistics(){
        path_length=0;
        cost=0;
        n_nodes_visited=0;
        greatest_depth=0;
    }

    private static void printStatistics() {
        System.out.println("dolzina poti: "+path_length);
        System.out.println("cena poti: "+cost);
        System.out.println("število obiskanih vozlišč: "+n_nodes_visited);
        System.out.println("največja globina: "+greatest_depth);
    }

    private static void drawSolution(int[][] labirinth, ArrayList<int[]> resitev) {
        drawOut(labirinth);
        StdDraw.setPenColor(Color.GREEN);
        for(int[]e : resitev){
            drawNode(labirinth,(double)e[0],(double)e[1]);
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
    public static ArrayList<int[]> DFS(int n, int m , int[][]maze, int[] start, int[] end, ArrayList<int[]> tresures){
        Random random = new Random();
        int r = random.nextInt(256);
        int g = random.nextInt(256);
        int b = random.nextInt(256);
        StdDraw.setPenColor(Color.YELLOW);

        Stack<int[]> st = new Stack<>();
        ArrayList<int[]> path = new ArrayList<>();
        boolean[][] visited = new boolean[n][m];
        int[][][] parent = new int[n][m][];

        visited[start[0]][start[1]] = true;
        st.push(start);
        greatest_depth = st.size();
        while(!st.isEmpty()){
            int[] curNode = st.peek();
            for(int[] tresure:tresures){
                if(visited[tresure[0]][tresure[1]]){
                    tresures.remove(tresure);
                    drawOut(maze);
                    ArrayList<int[]> tmp = DFS(n,m,maze,curNode,end,tresures);
                    for(int[]e : tmp){
                        path.add(e);
                    }
                    int[] cur = curNode;
                    // loop until we reach s (s is the only visited node with null parent)
                    while(parent[cur[0]][cur[1]] != null) {
                        path.add(cur);
                        cur = parent[cur[0]][cur[1]];
                    }
                    return path;
                }
            }
            if(maze[curNode[0]][curNode[1]]==-4&&tresures.isEmpty()){
                int[] cur = end;
                // loop until we reach s (s is the only visited node with null parent)
                while(parent[cur[0]][cur[1]] != null) {
                    path.add(cur);
                    cur = parent[cur[0]][cur[1]];
                }
                // reverse and return the path
                return path;
            }
            boolean found = false;
            for(int[] d : dir) {
                int i = curNode[0] + d[0];
                int j = curNode[1] + d[1];
                // visit the edge from u to (i, j)
                if(!visited[i][j] && maze[i][j] !=-1) {
                    n_nodes_visited++;
                    drawNode(maze, (double) i, (double) j);
                    // node (i, j) has not yet been visited and is not a wall, add it
                    st.push(new int[]{i, j});
                    if(st.size()>greatest_depth){
                        greatest_depth = st.size();
                    }
                    visited[i][j] = true;
                    // set the parent of (i, j) to be u
                    parent[i][j] = curNode;
                    found = true;
                    break;
                }
                }
            if(!found){
                st.pop();
            }
        }
        return path;
    }

    private static void drawNode(int[][] maze, double i, double j) {
        StdDraw.pause(speed);
        StdDraw.filledSquare(j / (maze.length - 1), 1 - i / (maze.length - 1), (double) 1 / (maze.length - 1) * 0.5);
        StdDraw.show();
    }
    public static ArrayList<int[]> getPath(int[]curNode,int[][][] parent){
        ArrayList<int[]> path = new ArrayList<>();
        path.add(curNode);
        while(parent[curNode[0]][curNode[1]] != null){
            curNode=parent[curNode[0]][curNode[1]];
            path.add(curNode);
        }
        path.remove(path.size()-1);
        return path;
    }
    static int h(int[] curNode,ArrayList<int[]>tresures,int[]end){
        int h=Integer.MAX_VALUE;
        if(tresures.isEmpty()){
            h=distance(curNode,end);
            return h;
        }
        for(int[] tresure:tresures){
            int tmp=distance(curNode,tresure);
            if(tmp<h){
                h=tmp;
            }
        }
        return h;
    }

    public static int distance(int[] point1, int[] point2) {
        return Math.abs((point2[0]-point1[0]))+Math.abs((point2[1]-point1[1]));
    }

    public static <Queue> ArrayList<int[]> AStar(int n, int m, int[][] maze, int[] start, int[] end, ArrayList<int[]> tresures){
        ArrayList<int[]> path = new ArrayList<>();
        ArrayList<int[]> Q = new ArrayList<>();
        Q.add(start);
        int[][][] parent = new int[n][m][];

        int[][] gScore = new int[n][m];
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < m; j++) {
                gScore[i][j] = Integer.MAX_VALUE;
            }
        }

        gScore[start[0]][start[1]]=0;

        int[][] fScore = new int[n][m];
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < m; j++) {
                fScore[i][j] = Integer.MAX_VALUE;
            }
        }
        fScore[start[0]][start[1]]=h(start,tresures,end);
        while(!Q.isEmpty()){
            int[] curNode = getMinNode(Q, fScore);
            if(maze[curNode[0]][curNode[1]]==-4&&tresures.isEmpty()){
                path=getPath(curNode,parent);
                Collections.reverse(path);
                for(int[] node:path){
                    cost+=fScore[node[0]][node[1]];
                }
                return path;
            }
            for(int[] tresure:tresures){
                if(tresure[0]==curNode[0]&&tresure[1]==curNode[1]) {
                    tresures.remove(tresure);
                    drawOut(maze);
                    ArrayList<int[]> tmp = AStar(n,m,maze,tresure,end,tresures);
                    path=getPath(curNode,parent);
                    Collections.reverse(path);
                    for(int[] node:path){
                        cost+=fScore[node[0]][node[1]];
                    }
                    for(int[] node:tmp){
                        path.add(node);
                    }
                    return path;
                }
            }
            Q.remove(curNode);
            for(int[] d : dir) {
                int i = curNode[0] + d[0];
                int j = curNode[1] + d[1];
                if(maze[i][j]==-1){
                    continue;
                }
                int t_gScore = gScore[curNode[0]][curNode[1]]+1;
                if(t_gScore<gScore[i][j]){
                    n_nodes_visited++;
                    StdDraw.setPenColor(Color.YELLOW);
                    drawNode(maze, (double) i, (double) j);
                    parent[i][j]=curNode;
                    gScore[i][j]=t_gScore;
                    if(gScore[i][j]>greatest_depth){
                        greatest_depth=gScore[i][j];
                    }
                    int[] neighbor = new int[]{i,j};
                    fScore[i][j]=t_gScore+h(neighbor,tresures,end);
                    if(!QContains(neighbor,Q)){
                        Q.add(neighbor);
                    }
                }
            }


        }
        return path;
    }


    private static boolean QContains(int[] neighbor, ArrayList<int[]> q) {
        for(int[] node : q){
            if(node[0]==neighbor[0]&&node[1]==neighbor[1])
                return true;
        }
        return false;
    }

    private static int[] getMinNode(ArrayList<int[]> Q, int[][] fScore) {
        int tmpF=Integer.MAX_VALUE;
        int[] curNode = new int[0];
        for(int[] node : Q){
            if(fScore[node[0]][node[1]]<tmpF){
                tmpF= fScore[node[0]][node[1]];
                curNode=node;
            }
        }
        return curNode;
    }
    public static <Queue> ArrayList<int[]> BFS(int n, int m, int[][] maze, int[] start, int[] end, ArrayList<int[]> tresures) throws InterruptedException {
        java.util.Queue<int[]> Q = new LinkedList<>();
        Q.add(start);
        boolean[][] visited = new boolean[n][m];
        int[][][] parent = new int[n][m][];
        visited[start[0]][start[1]] = true;
        while(!Q.isEmpty()) {
            int[] u = Q.poll();
            // we are now processing node u
            for(int[] d : dir) {
                int i = u[0] + d[0];
                int j = u[1] + d[1];
                if(!visited[i][j] && maze[i][j] !=-1) {
                    n_nodes_visited++;
                    StdDraw.setPenColor(Color.YELLOW);
                    drawNode(maze,(double) i , (double) j);
                    Q.add(new int[] {i, j});
                    visited[i][j] = true;
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
        for(int[] e : tresures){
            if(visited[e[0]][e[1]]) {
                tresures.remove(e);
                curend = e;
                break;
            }
        }

        ArrayList<int[]> path = new ArrayList<>();
        ArrayList<int[]> this_path = new ArrayList<>();
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
        int[] cur = curend;
        while(parent[cur[0]][cur[1]] != null) {
            this_path.add(cur);
            cur = parent[cur[0]][cur[1]];
        }
        if(this_path.size()>greatest_depth)
            greatest_depth = this_path.size();
        for(int[] e : this_path){
            path.add(e);
        }
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
