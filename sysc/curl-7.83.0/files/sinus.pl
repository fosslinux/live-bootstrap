my $pi = 3.1415;
foreach my $i (1 .. 200) {
    printf "%d, ", sin($i/200 * 2 * $pi) * 500000 + 500000;
}
