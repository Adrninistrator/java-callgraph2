package test.frame;

import com.adrninistrator.javacg2.dto.element.BaseElement;
import com.adrninistrator.javacg2.dto.element.constant.ConstElementString;
import com.adrninistrator.javacg2.dto.frame.JavaCG2OperandStack;
import com.adrninistrator.javacg2.util.JavaCG2ElementUtil;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

/**
 * @author codegen
 * @description: 验证 FrameSnapshotsOfIhs 快照比较的"跳过已 matched 下标"优化不改变结果。
 *
 * 优化内容（待应用到 JavaCG2OperandStack/JavaCG2LocalVariables/FieldInformationMap 的 compareLooseMode）：
 *   for (int i = 0; i < n; i++) {
 *       if (sameStackSeqSet.contains(i)) {   // 新增：跳过已 matched 下标
 *           continue;
 *       }
 *       ... sameStackSeqSet.add(i) ...
 *   }
 *
 * 验证方法：差分对比三路。
 *   A: 真实 JavaCG2OperandStack.compareLooseMode（当前生产实现，无 skip）。
 *   B: 本地复刻的无 skip 实现（reference）。
 *   C: 本地复刻的带 skip 实现（candidate）。
 * 三路各自在"多个已存在快照 + 共享累积 Set"的循环（镜像 checkExistSnapshotLooseMode 的栈维度累积与"是否已存在相同快照"判定）中运行，
 * 断言 A==B（本地复刻与生产一致，证明复刻忠实）且 B==C（skip 不改变最终 Set 与 dup 判定）。
 *
 * 说明：skip 优化对 栈/本地变量/非静态字段/静态字段 四个维度的逻辑一致且相互独立，
 * 对栈维度证明等价即等价于对每个维度证明等价，进而等价于对 checkExistSnapshotLooseMode 整体证明。
 *
 * 运行：./gradlew test --tests "test.frame.TestCompareLooseModeSkip"
 */
public class TestCompareLooseModeSkip {

    /**
     * 校验 compare 对同值/异值 ConstElementString 的行为，确认输入语义：同值=匹配，异值=不匹配。
     * （三路均使用同一个 JavaCG2ElementUtil.compare，故其内部行为不影响等价性结论，此断言仅为自证输入语义。）
     */
    @Test
    public void compareSanity() {
        assertTrue(JavaCG2ElementUtil.compare(new ConstElementString("a"), new ConstElementString("a")));
        assertFalse(JavaCG2ElementUtil.compare(new ConstElementString("a"), new ConstElementString("b")));
    }

    @Test
    public void accumulationEquivalence() {
        // 1. 无任何匹配（dup=false，Set 始终空，skip 不触发）
        runCase(list(stack("x", "y")), stack("a", "b"));
        // 2. 第一个已存在快照即完全匹配（dup=true at #0，skip 未触发即已判定）
        runCase(list(stack("a", "b")), stack("a", "b"));
        // 3. 跨两个快照的并集匹配（关键 skip 场景：#0 命中 idx0，#1 命中 idx1 → #1 跳过 idx0）
        runCase(list(stack("a", "x"), stack("y", "b")), stack("a", "b"));
        // 4. 顺序倒置的并集匹配（#0 命中 idx1，#1 命中 idx0）
        runCase(list(stack("y", "b"), stack("a", "x")), stack("a", "b"));
        // 5. 跨三个快照的并集匹配（#1 跳过 idx0，#2 跳过 idx0/idx1）
        runCase(list(stack("a", "p", "q"), stack("r", "b", "p"), stack("q", "r", "c")), stack("a", "b", "c"));
        // 6. 部分匹配但永不完整（dup=false，skip 在后续快照对已 matched 下标触发）
        runCase(list(stack("a", "x"), stack("y", "z")), stack("a", "b"));
        // 7. 尺寸不一致（compareLooseMode 提前 return，Set 不变）
        runCase(list(stack("a", "b", "c")), stack("a", "b"));
        // 8. 空已存在快照列表（dup=false）
        runCase(list(), stack("a", "b"));
        // 9. 单元素栈：#0 不匹配、#1 匹配 → dup=true at #1
        runCase(list(stack("b"), stack("a")), stack("a"));
        // 10. 三元素、每快照只命中一个、第三快照才完整（skip 在 #1/#2 反复触发）
        runCase(list(stack("a", "x", "x"), stack("x", "b", "x"), stack("x", "x", "c")), stack("a", "b", "c"));
    }

    /**
     * 优化效果测试：统计 compare()（重方法）调用次数，证明 skip 减少对"已 matched 下标"的重复 compare。
     * 因 JavaCG2ElementUtil.compare 为 static 无法注入计数，这里在本地复刻路径中计数；
     * 由 accumulationEquivalence 的 A==B 已证明本地复刻忠实于生产 compareLooseMode，故本地计数即代表生产 compare 调用次数。
     * 断言：skip 路径 compare 次数 <= 无 skip（永不多算），并集匹配场景严格 <（确实省了）。
     */
    @Test
    public void optimizationEffect_compareCallCount() {
        // expectStrictLess=true：存在某下标在早于"完成快照"的先前快照中已 matched，skip 会真正触发
        assertEffect(list(stack("x", "y")), stack("a", "b"), false);                                               // 1 无匹配：Set 始终空，skip 不触发
        assertEffect(list(stack("a", "b")), stack("a", "b"), false);                                                 // 2 首快照即全匹配：单快照内无 skip
        assertEffect(list(stack("a", "x"), stack("y", "b")), stack("a", "b"), true);                                // 3 并集2：#1 跳过 idx0
        assertEffect(list(stack("y", "b"), stack("a", "x")), stack("a", "b"), true);                                // 4 并集2倒置：#1 跳过 idx1
        assertEffect(list(stack("a", "p", "q"), stack("r", "b", "p"), stack("q", "r", "c")), stack("a", "b", "c"), true); // 5 并集3
        assertEffect(list(stack("a", "x"), stack("y", "z")), stack("a", "b"), true);                               // 6 部分匹配：#1 跳过 idx0
        assertEffect(list(stack("a", "b", "c")), stack("a", "b"), false);                                          // 7 尺寸不一致：提前 return，count 0
        assertEffect(list(), stack("a", "b"), false);                                                               // 8 空列表：count 0
        assertEffect(list(stack("b"), stack("a")), stack("a"), false);                                              // 9 单元素仅末快照命中：Set 末才非空，无 skip
        assertEffect(list(stack("a", "x", "x"), stack("x", "b", "x"), stack("x", "x", "c")), stack("a", "b", "c"), true); // 10 并集3
    }

    private void assertEffect(List<JavaCG2OperandStack> existedList, JavaCG2OperandStack added, boolean expectStrictLess) {
        int addedSize = added.size();
        Counter noSkipCounter = new Counter();
        Counter withSkipCounter = new Counter();
        AccumResult b = accumulateCounted(existedList, added, addedSize, false, noSkipCounter);
        AccumResult c = accumulateCounted(existedList, added, addedSize, true, withSkipCounter);
        // 正确性：结果一致
        assertEquals("effect: matchedSet 一致", b.matchedSet, c.matchedSet);
        assertEquals("effect: dup 一致", b.dup, c.dup);
        // 优化效果：skip 的 compare 调用次数 <= 无 skip（永不多算）
        assertTrue("withSkip(" + withSkipCounter.count + ") 应 <= noSkip(" + noSkipCounter.count + ")",
                withSkipCounter.count <= noSkipCounter.count);
        if (expectStrictLess) {
            assertTrue("并集匹配场景 withSkip(" + withSkipCounter.count + ") 应严格 < noSkip(" + noSkipCounter.count + ")",
                    withSkipCounter.count < noSkipCounter.count);
        }
        System.out.println("effect: noSkip.compareCalls=" + noSkipCounter.count + " withSkip.compareCalls=" + withSkipCounter.count
                + " (strictLessExpected=" + expectStrictLess + ")");
    }

    private static AccumResult accumulateCounted(List<JavaCG2OperandStack> existedList, JavaCG2OperandStack added, int addedSize,
                                                 boolean skipMatched, Counter counter) {
        Set<Integer> matchedSet = new HashSet<>();
        boolean dup = false;
        for (JavaCG2OperandStack existed : existedList) {
            if (addedSize != matchedSet.size()) {
                compareLocalCounted(existed, added, matchedSet, skipMatched, counter);
            }
            if (addedSize == matchedSet.size()) {
                dup = true;
                break;
            }
        }
        return new AccumResult(matchedSet, dup);
    }

    private static void compareLocalCounted(JavaCG2OperandStack existed, JavaCG2OperandStack added, Set<Integer> sameStackSeqSet,
                                            boolean skipMatched, Counter counter) {
        if (existed.size() != added.size()) {
            return;
        }
        int n = added.size();
        for (int i = 0; i < n; i++) {
            if (skipMatched && sameStackSeqSet.contains(i)) {
                continue;
            }
            BaseElement ae = added.getFromTop(n - 1 - i);
            BaseElement ee = existed.getFromTop(n - 1 - i);
            if (ae == null || (ee != null && countedCompare(ee, ae, counter))) {
                sameStackSeqSet.add(i);
            }
        }
    }

    private static boolean countedCompare(BaseElement ee, BaseElement ae, Counter counter) {
        counter.count++;
        return JavaCG2ElementUtil.compare(ee, ae);
    }

    private static final class Counter {
        long count = 0;
    }

    /**
     * 墙钟微基准（合成压力场景）：N 个已存在快照 + N 元素 added，快照 k 仅命中 idx k
     * （idx 0..k-1 已在累积 Set 中 → 被 skip）。
     * 无 skip 每快照比较 N 个 → N*N 次；带 skip → N+(N-1)+...+1 ≈ N²/2 次（约省 50% compare 调用）。
     *
     * 诚实声明：这是合成压力场景，专门让 skip 高频触发以观测时间差；
     *   - 用 ConstElementString（compare 较廉价）会低估真实节省（真实符号执行里 compare 含递归数据来源比较，更重）；
     *   - 本地复刻用 getFromTop 间接读 elements，绝对时间略高于生产，但 B/C 同用 getFromTop，时间"比值"仍代表 skip 效果（间接开销相互抵消）；
     *   - 真实 jar 的收益取决于有多少方法走到多快照累积路径（典型方法几乎无收益，病态方法接近此场景）。
     * 仅打印时间，不对时间做断言（CI 抖动）；compare 调用次数做确定性断言。
     */
    @Test
    public void optimizationEffect_wallClock() {
        int n = 250;
        int warmup = 3;
        int iter = 15;

        JavaCG2OperandStack added = stackOfDistinct(n);
        List<JavaCG2OperandStack> existed = matchOnePerSnapshot(n);

        // 预热
        for (int i = 0; i < warmup; i++) {
            accumulateCounted(existed, added, n, false, new Counter());
            accumulateCounted(existed, added, n, true, new Counter());
        }

        // 计时 no-skip
        Counter cNo = new Counter();
        long tNo = 0L;
        for (int i = 0; i < iter; i++) {
            long s = System.nanoTime();
            accumulateCounted(existed, added, n, false, cNo);
            tNo += System.nanoTime() - s;
        }
        // 计时 with-skip
        Counter cSkip = new Counter();
        long tSkip = 0L;
        for (int i = 0; i < iter; i++) {
            long s = System.nanoTime();
            accumulateCounted(existed, added, n, true, cSkip);
            tSkip += System.nanoTime() - s;
        }

        double noMs = tNo / 1_000_000.0 / iter;
        double skipMs = tSkip / 1_000_000.0 / iter;
        double callsNo = cNo.count / (double) iter;
        double callsSkip = cSkip.count / (double) iter;
        double timeReduction = (1.0 - skipMs / noMs) * 100.0;
        double callReduction = (1.0 - callsSkip / callsNo) * 100.0;
        System.out.printf("wallClock stress(n=%d, iter=%d): noSkip=%.3fms withSkip=%.3fms timeReduction=%.1f%% | "
                        + "compareCalls: noSkip=%.0f withSkip=%.0f callReduction=%.1f%%%n",
                n, iter, noMs, skipMs, timeReduction, callsNo, callsSkip, callReduction);

        // 确定性断言：skip 的 compare 调用次数严格少于无 skip（约省 50%）
        assertTrue("withSkip calls(" + cSkip.count + ") 应严格 < noSkip calls(" + cNo.count + ")", cSkip.count < cNo.count);
    }

    /** added = [v0, v1, ..., v(n-1)]（每个值唯一） */
    private static JavaCG2OperandStack stackOfDistinct(int n) {
        JavaCG2OperandStack s = new JavaCG2OperandStack(n);
        for (int i = 0; i < n; i++) {
            s.push(new ConstElementString("v" + i));
        }
        return s;
    }

    /** 快照 k：idx k = "vk"（与 added[k] 相同），其余 idx = "X"（不匹配）→ 每快照仅命中 idx k */
    private static List<JavaCG2OperandStack> matchOnePerSnapshot(int n) {
        List<JavaCG2OperandStack> list = new ArrayList<>(n);
        for (int k = 0; k < n; k++) {
            JavaCG2OperandStack s = new JavaCG2OperandStack(n);
            for (int i = 0; i < n; i++) {
                s.push(new ConstElementString((i == k) ? ("v" + i) : "X"));
            }
            list.add(s);
        }
        return list;
    }

    /**
     * 对一组已存在快照与一个待判定的 added，分别用 真实(无skip) / 本地(无skip) / 本地(带skip) 三路累积，
     * 断言三路最终 Set 与 dup 判定一致。
     */
    private void runCase(List<JavaCG2OperandStack> existedList, JavaCG2OperandStack added) {
        int addedSize = added.size();

        AccumResult a = accumulate(existedList, added, addedSize, AccumMode.REAL_NO_SKIP);
        AccumResult b = accumulate(existedList, added, addedSize, AccumMode.LOCAL_NO_SKIP);
        AccumResult c = accumulate(existedList, added, addedSize, AccumMode.LOCAL_WITH_SKIP);

        // A==B：本地复刻与生产一致（证明复刻忠实，否则后续 B==C 无意义）
        assertEquals("A==B matchedSet 本地复刻与生产不一致", a.matchedSet, b.matchedSet);
        assertEquals("A==B dup 本地复刻与生产不一致", a.dup, b.dup);
        // B==C：skip 不改变结果（核心结论）
        assertEquals("B==C matchedSet skip 改变了结果", b.matchedSet, c.matchedSet);
        assertEquals("B==C dup skip 改变了结果", b.dup, c.dup);
    }

    private enum AccumMode { REAL_NO_SKIP, LOCAL_NO_SKIP, LOCAL_WITH_SKIP }

    private static class AccumResult {
        final Set<Integer> matchedSet;
        final boolean dup;

        AccumResult(Set<Integer> matchedSet, boolean dup) {
            this.matchedSet = matchedSet;
            this.dup = dup;
        }
    }

    /**
     * 镜像 FrameSnapshotsOfIhs.checkExistSnapshotLooseMode 的栈维度累积逻辑：
     * 顺序遍历已存在快照，共享同一个累积 Set；只要 Set 尚未覆盖全部 added 下标就继续 compareLooseMode；
     * 一旦覆盖全部即判定为"已存在相同快照"(dup=true) 并停止。
     */
    private static AccumResult accumulate(List<JavaCG2OperandStack> existedList, JavaCG2OperandStack added, int addedSize, AccumMode mode) {
        Set<Integer> matchedSet = new HashSet<>();
        boolean dup = false;
        for (JavaCG2OperandStack existed : existedList) {
            if (addedSize != matchedSet.size()) {
                switch (mode) {
                    case REAL_NO_SKIP:
                        JavaCG2OperandStack.compareLooseMode(existed, added, matchedSet);
                        break;
                    case LOCAL_NO_SKIP:
                        compareLocal(existed, added, matchedSet, false);
                        break;
                    case LOCAL_WITH_SKIP:
                        compareLocal(existed, added, matchedSet, true);
                        break;
                }
            }
            if (addedSize == matchedSet.size()) {
                dup = true;
                break;
            }
        }
        return new AccumResult(matchedSet, dup);
    }

    /**
     * 本地复刻 compareLooseMode：与生产实现唯一可选差异为是否跳过已 matched 下标。
     * 因 JavaCG2OperandStack.elements 为 private，这里通过 getFromTop(n-1-i) 读取 elements[i]
     * （getFromTop(interval) 返回 elements[head-1-interval]，head=n 时即 elements[i]），与生产直接访问私有数组等价。
     */
    private static void compareLocal(JavaCG2OperandStack existed, JavaCG2OperandStack added, Set<Integer> sameStackSeqSet, boolean skipMatched) {
        if (existed.size() != added.size()) {
            return;
        }
        int n = added.size();
        for (int i = 0; i < n; i++) {
            if (skipMatched && sameStackSeqSet.contains(i)) {
                continue;
            }
            BaseElement ae = added.getFromTop(n - 1 - i);
            BaseElement ee = existed.getFromTop(n - 1 - i);
            if (ae == null || (ee != null && JavaCG2ElementUtil.compare(ee, ae))) {
                sameStackSeqSet.add(i);
            }
        }
    }

    private static JavaCG2OperandStack stack(String... values) {
        JavaCG2OperandStack s = new JavaCG2OperandStack(Math.max(1, values.length));
        for (String v : values) {
            s.push(new ConstElementString(v));
        }
        return s;
    }

    private static List<JavaCG2OperandStack> list(JavaCG2OperandStack... stacks) {
        return Arrays.asList(stacks);
    }
}
