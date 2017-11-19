package fi.kapsi.kosmik.sfti;

import static fi.kapsi.kosmik.sfti.Chapter15.ex04_sum;

public class Chapter15Test {
    private void ex04() {
        System.out.println(ex04_sum(2, 3, 7));
    }

    public static void main(String... args) {
        Chapter15Test test = new Chapter15Test();
        test.ex04();
    }
}
