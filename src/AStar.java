import java.awt.*;
import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Scanner;

public class AStar {
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
            drawOut(labirinth);
            ArrayList<int[]> resitev = AStar(labirinth.length,labirinth.length,labirinth,start,end,tresures);
            Collections.reverse(resitev);
            drawOut(labirinth);
            StdDraw.setPenColor(Color.GREEN);
            for(int[]e : resitev){
                StdDraw.filledSquare((double) e[1] / (labirinth.length - 1), 1- (double) e[0] / (labirinth.length - 1), (double) 1 / (labirinth.length - 1) * 0.5);
                StdDraw.pause(10);
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
                return path;
            }
            for(int[] tresure:tresures){
                if(tresure[0]==curNode[0]&&tresure[1]==curNode[1]) {
                    tresures.remove(tresure);
                    drawOut(maze);
                    ArrayList<int[]> tmp = AStar(n,m,maze,tresure,end,tresures);
                    path=getPath(curNode,parent);
                    Collections.reverse(path);
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
                    drawNode(maze, (double) i, (double) j);
                    parent[i][j]=curNode;
                    gScore[i][j]=t_gScore;
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

    private static void drawNode(int[][] maze, double i, double j) {
        StdDraw.setPenColor(Color.YELLOW);
        StdDraw.pause(10);
        StdDraw.filledSquare(j / (maze.length - 1), 1 - i / (maze.length - 1), (double) 1 / (maze.length - 1) * 0.5);
        StdDraw.show();
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
}
