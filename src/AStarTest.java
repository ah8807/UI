import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class AStarTest {

    @Test
    void testDistance() {
        int[] stev1 = new int[]{2,4};
        int[] stev2 = new int[]{4,2};
        assertEquals(4,distance(stev1,stev2));
        stev1 = new int[]{-2,-4};
        stev2 = new int[]{4,2};
        assertEquals(12,distance(stev1,stev2));
        stev1 = new int[]{-2,4};
        stev2 = new int[]{4,-2};
        assertEquals(12,distance(stev1,stev2));
        stev1 = new int[]{2,-4};
        stev2 = new int[]{-4,2};
        assertEquals(12,distance(stev1,stev2));
    }

    public static int distance(int[] point1, int[] point2) {
        return Math.abs((point2[0]-point1[0]))+Math.abs((point2[1]-point1[1]));
    }
}