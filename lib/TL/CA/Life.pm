package TL::CA::Life;
use strict;
use warnings;

=head1 ACKNOWLEDGEMENTS

This code is adapted from the perlmonks node "More Betterer Game of Life"
(https://www.perlmonks.org/?node_id=1200097), from perlmonks user
'eyepopslikeamosquito'.

=cut

our @ISA = qw{TL::CA};

sub new {
    my($class, $init) = @_;
    my $self = bless {
        Tiles => {},
        Modified => [],
    }, $class;
    if ($init) {
        for my $x (0 .. $#$init) {
            my $sx = $init->[$x];
            for my $y (0 .. length($sx) - 1) {
                next if substr($sx, $y, 1) ne '1';
                $self->write_cell($x, $y, 1);
            }
        }
    }
    return $self;
}

# Instead of a hashref, these list indices are slightly faster
my $Tx          = 0;
my $Ty          = 1;
my $Updateflags = 2;
my $Row         = 3;
my $Neighbours  = 4;

sub read_cell {
    my($self, $x, $y) = @_;
    my $tiles = $self->{Tiles};
    my($tx, $ty, $ix, $iy) = get_tile_coords($x, $y);
    my $k = pack 'i2', $tx, $ty;
    return 0 unless $tiles->{$k};
    return st_getcellval($tiles->{$k}->[$Row], $ix, $iy) ? 1 : 0;
}

sub write_cell {
    my($self, $x, $y, $state) = @_;
    my $tiles = $self->{Tiles};
    my($tx, $ty, $ix, $iy) = get_tile_coords($x, $y);
    my $k = pack 'i2', $tx, $ty;
    $tiles->{$k} //= st_newtile($tx, $ty);
    st_setcellval($tiles->{$k}->[$Row], $ix, $iy, $state);
    $self->updatecell($tiles->{$k}, $ix, $iy);
    return;
}

# ----------------------------------------------------------------
# The Universe is modelled as a set of overlapping tiles.
# For background, see http://conwaylife.com/wiki/Life128_and_vlife
# We use a simple scheme of 64 x 64 tiles (60 x 60 core) with
# conventional tiling (each tile has eight neighbours).
# Alternatively 32 x 32 (28 x 28 core) are used with 32-bit integers.
# Note that this was chosen for simplicity; more efficient schemes
# are available, such as the "brick wall tiling" used by Goucher
# in later versions (apgmera, version 3)
# This code is loosely based on apgnano (version 2).
# ----------------------------------------------------------------
# SQUARE TILE
# Note: 64 x 64 square tiles require a perl built with 64-bit integers
# Choose tile size (32 or 64) automatically based on perl integer size:
use Config;
our $TILE_SIZE_FULL = $Config{ivsize} < 8 ? 32 : 64;
# ... or manually override by editing the next line
# $TILE_SIZE_FULL = 32;    # manually set to 32 or 64
# warn __PACKAGE__, ": using $TILE_SIZE_FULL x $TILE_SIZE_FULL tiles\n";

my $BM_MIDDLE      = 0x3ffffffc;
my $BM_LEFT        = 0xfffffffc;
my $BM_RIGHT       = 0x3fffffff;
my $BM_OUTER       = 0xc0000003;
my $BM_LEFTMIDDLE  = 0x30000000;
my $BM_RIGHTMIDDLE = 0x0000000c;
my $BM_FMT         = '%032b';
if ($TILE_SIZE_FULL == 64) {
    no warnings qw(portable overflow);
    $BM_MIDDLE      = 0x3ffffffffffffffc;
    $BM_LEFT        = 0xfffffffffffffffc;
    $BM_RIGHT       = 0x3fffffffffffffff;
    $BM_OUTER       = 0xc000000000000003;
    $BM_LEFTMIDDLE  = 0x3000000000000000;
    $BM_RIGHTMIDDLE = 0x000000000000000c;
    $BM_FMT         = '%064b';
}

my $BORDER_WIDTH       = 2;
my $BORDER_WIDTH_P1    = $BORDER_WIDTH + 1;
my $TILE_SIZE_FULL_M1  = $TILE_SIZE_FULL - 1;
my $TILE_SIZE_FULL_M2  = $TILE_SIZE_FULL - 2;
my $TILE_SIZE_MBD      = $TILE_SIZE_FULL - $BORDER_WIDTH;
my $TILE_SIZE_MBD_M1   = $TILE_SIZE_MBD - 1;
my $TILE_SIZE_CORE     = $TILE_SIZE_FULL - 2 * $BORDER_WIDTH;
my $TILE_SIZE_CORE_P1  = $TILE_SIZE_CORE + 1;

# Neighbours are numbered clockwise starting with the one directly above
my $NUM_NEIGH          = 8;
my $NUM_NEIGH_M1       = $NUM_NEIGH - 1;
my $NEIGH_TOP          = 0;
my $NEIGH_TOP_RIGHT    = 1;
my $NEIGH_RIGHT        = 2;
my $NEIGH_BOTTOM_RIGHT = 3;
my $NEIGH_BOTTOM       = 4;
my $NEIGH_BOTTOM_LEFT  = 5;
my $NEIGH_LEFT         = 6;
my $NEIGH_TOP_LEFT     = 7;
my $NEIGH_ANY          = 0xff;    # test if any neighbour is set
# Note that the i ^ 4 trick sets i to the opposite one, that is:
#   0 > 4  (TOP > BOTTOM)
#   1 > 5  (TOP RIGHT > BOTTOM LEFT)
#   2 > 6  (RIGHT > LEFT)
#   3 > 7  (BOTTOM RIGHT > TOP LEFT)
#   4 > 0  (BOTTOM > TOP)
#   5 > 1  (BOTTOM LEFT > TOP RIGHT)
#   6 > 2  (LEFT > RIGHT)
#   7 > 3  (TOP LEFT > BOTTOM RIGHT)

# The functions starting with st_ manipulate
# a simple $TILE_SIZE_FULL x $TILE_SIZE_FULL square tile bitmap.
# Note that $x and $y must be in 0..$TILE_SIZE_FULL-1 range.
# $row is a ref to an array of 64 unsigned 64-bit ints
#  ... or a ref to an array of 32 unsigned 32-bit ints.
# The value in row[] bitmap is 0 (dead) or 1 (alive).

sub st_newtile {
    my($tx, $ty) = @_;
    # Tx, Ty, Updateflags, Row, Neighbours
    return [ $tx, $ty, 0, [ (0) x $TILE_SIZE_FULL ], [] ];
}

sub st_getcellval {
    my($row, $x, $y) = @_;
    # my $mask = 1 << ($TILE_SIZE_FULL_M1 - $x);
    # return $row->[$y] & $mask ? 1 : 0;
    return $row->[$y] & (1 << ($TILE_SIZE_FULL_M1 - $x));
}

sub st_setcellval {
    my($row, $x, $y, $v) = @_;
    my $mask = 1 << ($TILE_SIZE_FULL_M1 - $x);
    if ($v) {
        $row->[$y] |= $mask;
    } else {
        $row->[$y] &= ~$mask;
    }
    return;
}

sub st_insertcells {
    my $row = shift;
    st_setcellval($row, $_->[0], $_->[1], 1) for @_;
    return;
}

# Used for verification and unit testing of st_tiletick
sub st_count {
    my $row = shift;
    my $cnt = 0;
    for my $y (0 .. $TILE_SIZE_FULL_M1) {
        next unless $row->[$y];
        # $cnt += popcount($row->[$y]);
        $cnt += sprintf('%b', $row->[$y]) =~ tr/1//;
    }
    return $cnt;
}

# Used for verification and unit testing of st_tiletick
sub st_getlivecells {
    my $row = shift;
    my @cells;
    for my $y (0 .. $TILE_SIZE_FULL_M1) {
        next unless $row->[$y];
        for my $x (0 .. $TILE_SIZE_FULL_M1) {
            push @cells, [$x, $y] if st_getcellval($row, $x, $y);
        }
    }
    return sort { $a->[0] <=> $b->[0] || $a->[1] <=> $b->[1] } @cells;
}

# Advance the interior of square tile by one tick.
# Return a two element list:
#   [0] : boolean: TRUE if square tile changed, else FALSE
#   [1] : neighbour flags (see NEIGH flags above)
#         indicates which neighbours need to be updated
# $top    is index of first non-zero element in $row
# $bottom is index of last  non-zero element in $row
# Note: Do not call this function with all elements zero
#  .... and it is pointless to do so
sub st_tiletick {
    my($row, $top, $bottom) = @_;

    my $neigh = my $bigdiff = 0;
    my @carry = my @parity = my @diff = my @ee = (0) x $TILE_SIZE_FULL;
    my($aa, $bb, $p, $q, $r, $s, $bit0, $bit1, $bit2);

    for my $i ($top .. $bottom) {
        $aa = $row->[$i] >> 1;
        $bb = $row->[$i] << 1;
        $q = $aa ^ $bb;
        $parity[$i] = $q ^ $row->[$i];
        $carry[$i] = ($q & $row->[$i]) | ($aa & $bb);
    }
    --$top;
    ++$bottom;
    $top = 1 if $top < 1;
    $bottom = $TILE_SIZE_MBD if $bottom > $TILE_SIZE_MBD;
    for my $i ($top .. $bottom) {
        $aa = $parity[$i - 1];
        $bb = $parity[$i + 1];
        $q = $aa ^ $bb;
        $bit0 = $q ^ $parity[$i];
        $r = ($q & $parity[$i]) | ($aa & $bb);

        $aa = $carry[$i - 1];
        $bb = $carry[$i + 1];
        $q = $aa ^ $bb;
        $p = $q ^ $carry[$i];
        $s = ($q & $carry[$i]) | ($aa & $bb);

        $bit1 = $p ^ $r;
        $bit2 = $s ^ ($p & $r);
        $p = ($bit0 & $bit1 & ~$bit2) | ($bit2 & ~$bit1 & ~$bit0 & $row->[$i]);
        $diff[$i] = ($row->[$i] ^ $p) & $BM_MIDDLE;
        $bigdiff |= $diff[$i];
        $row->[$i] = ($p & $BM_MIDDLE) | ($row->[$i] & ~$BM_MIDDLE);
    }
    $aa = $diff[$BORDER_WIDTH]   | $diff[$BORDER_WIDTH_P1];
    $bb = $diff[$TILE_SIZE_CORE] | $diff[$TILE_SIZE_CORE_P1];
    if ($bigdiff) {
        $neigh |= 1 << $NEIGH_LEFT  if $bigdiff & $BM_LEFTMIDDLE;
        $neigh |= 1 << $NEIGH_RIGHT if $bigdiff & $BM_RIGHTMIDDLE;
    }
    if ($aa) {
        $neigh |= 1 << $NEIGH_TOP;
        $neigh |= 1 << $NEIGH_TOP_LEFT  if $aa & $BM_LEFTMIDDLE;
        $neigh |= 1 << $NEIGH_TOP_RIGHT if $aa & $BM_RIGHTMIDDLE;
    }
    if ($bb) {
        $neigh |= 1 << $NEIGH_BOTTOM;
        $neigh |= 1 << $NEIGH_BOTTOM_LEFT  if $bb & $BM_LEFTMIDDLE;
        $neigh |= 1 << $NEIGH_BOTTOM_RIGHT if $bb & $BM_RIGHTMIDDLE;
    }
    return +($bigdiff, $neigh);
}

# Note: mapping of x (cell) to tx (tile) is:
#        x       tx
#   ----------   --
#   ...
#   -121..-180   -3
#    -61..-120   -2
#     -1..-60    -1
#      0.. 59     0
#     60..119     1
#    120..179     2
#   ...
# Ditto for y (cell) to ty (tile).

# Input cell (x, y). Return (tx, ty, ix, iy)
# (tx, ty) : Tile coords
# (ix, iy) : x, y coords inside tile
sub get_tile_coords {
    my($x, $y) = @_;
    my $ox = $x % $TILE_SIZE_CORE;
    my $oy = $y % $TILE_SIZE_CORE;
    $ox += $TILE_SIZE_CORE if $ox < 0;
    $oy += $TILE_SIZE_CORE if $oy < 0;
    my $tx = ($x - $ox) / $TILE_SIZE_CORE;
    my $ty = ($y - $oy) / $TILE_SIZE_CORE;
    my $ix = $ox + $BORDER_WIDTH;
    my $iy = $oy + $BORDER_WIDTH;
    return +($tx, $ty, $ix, $iy);
}

# Converse of get_tile_coords
# Input (tx, ty, ix, iy). Return cell (x, y)
sub get_cell_coords {
    my($tx, $ty, $ix, $iy) = @_;
    my $x = $TILE_SIZE_CORE * $tx + $ix - $BORDER_WIDTH;
    my $y = $TILE_SIZE_CORE * $ty + $iy - $BORDER_WIDTH;
    return +($x, $y);
}

# See perlmonks.org, node_id: 1199987
# Inline this popcount function below
# sub popcount { sprintf('%b', shift) =~ tr/1// }

# ----------------------------------------------------------------
# ORGANISM

sub dump_one_tile {
    my $sqt = shift;
    my $tx = $sqt->[$Tx];
    my $ty = $sqt->[$Ty];
    my $updateflags = $sqt->[$Updateflags];
    my $row = $sqt->[$Row];
    my $ngh = $sqt->[$Neighbours];
    my $popc = 0;
    for my $iy ($BORDER_WIDTH .. $TILE_SIZE_MBD_M1) {
        next unless $row->[$iy];
        # $cnt += popcount($row->[$iy] & $BM_MIDDLE);
        $popc += sprintf('%b', $row->[$iy] & $BM_MIDDLE) =~ tr/1//;
    }
    my $uf = sprintf '%032b', $updateflags;
    print STDERR "tx=$tx ty=$ty popc=$popc\n";
    print STDERR "  updateflags=$uf\n";
    print STDERR "  live neighbours:";
    for my $n (0 .. $NUM_NEIGH_M1) {
        $ngh->[$n] or next;
        print STDERR " $n";
    }
    print STDERR "\n";
}

# Used for verification and testing the state of the organism
sub dump_tiles {
    my $self = shift;
    my $tiles = $self->{Tiles};
    my @tkeys = sort keys %$tiles;
    my $ntiles = @tkeys;
    print STDERR "=== Dump Tiles, n=$ntiles ==========\n";
    my $ii = 0;
    for my $k (@tkeys) {
        ++$ii;
        my($kx, $ky) = unpack 'i2', $k;    # Note: $kx,$ky match Tx,Ty
        print STDERR "$ii:-----------------------------\n";
        my $sqt = $tiles->{$k};
        dump_one_tile($sqt);
    }
    my $modified = $self->{Modified};
    my $nmodified = @$modified;
    print STDERR "=== Dump Modif, n=$nmodified ==========\n";
    $ii = 0;
    for my $sqt (@$modified) {
        ++$ii;
        print STDERR "$ii:-----------------------------\n";
        dump_one_tile($sqt);
    }
    return;
}

# Used for verification and testing the state of the organism
sub get_live_cells {
    my $self = shift;
    my $tiles = $self->{Tiles};
    my @cells;
    for my $sqt (values %$tiles) {
        my($tx, $ty, $row) = @$sqt[$Tx, $Ty, $Row];
        for my $iy ($BORDER_WIDTH .. $TILE_SIZE_MBD_M1) {
            my $rowval = $row->[$iy];
            next unless $rowval;
            for my $ix ($BORDER_WIDTH .. $TILE_SIZE_MBD_M1) {
                # next unless st_getcellval($row, $ix, $iy);
                next unless $rowval & (1 << ($TILE_SIZE_FULL_M1 - $ix));
                push @cells, [
                    $TILE_SIZE_CORE * $tx + $ix - $BORDER_WIDTH,
                    $TILE_SIZE_CORE * $ty + $iy - $BORDER_WIDTH,
                ];
            }
        }
    }
    return sort { $a->[0] <=> $b->[0] || $a->[1] <=> $b->[1] } @cells;
}

# Assumes initial $sqt->[$Neighbours]->[$i] check has been done
sub get_neighbour {
    my $self = shift;
    my $sqt = shift;
    my $i  = shift;
    # $sqt->[$Neighbours]->[$i] and return $sqt->[$Neighbours]->[$i];
    my($tx, $ty) = @$sqt[$Tx, $Ty];
    ++$tx if $i >= $NEIGH_TOP_RIGHT    && $i <= $NEIGH_BOTTOM_RIGHT;
    ++$ty if $i >= $NEIGH_BOTTOM_RIGHT && $i <= $NEIGH_BOTTOM_LEFT;
    --$tx if $i >= $NEIGH_BOTTOM_LEFT  && $i <= $NEIGH_TOP_LEFT;
    --$ty if $i == $NEIGH_TOP_LEFT     || $i <= $NEIGH_TOP_RIGHT;
    my $tiles = $self->{Tiles};
    my $k = pack 'i2', $tx, $ty;
    return $sqt->[$Neighbours]->[$i] = $tiles->{$k} //= st_newtile($tx, $ty);
}

# Alert the neighbour that its neighbour (the original tile) has changed
sub update_neighbour {
    my $self = shift;
    my $sqt = shift;
    my $i  = shift;
    my $n = $sqt->[$Neighbours]->[$i] || $self->get_neighbour($sqt, $i);
    push @{$self->{Modified}}, $n if $n->[$Updateflags] == 0;
    $n->[$Updateflags] |= 1 << ($i ^ 4);
    return;
}

# Update the relevant portions of the boundary (a 64-by-64 square
# with the central 60-by-60 square removed) by copying data from
# the interiors (the 60-by-60 central squares) of the neighbours.
# Alternatively, for 32-bit ints, 32-by-32 with 28-by-28 core.
# Only perform this copying when necessary.
sub update_boundary {
    my $self = shift;
    my $sqt = shift;
    if ($sqt->[$Updateflags] & (1 << $NEIGH_TOP)) {
        my $n = $sqt->[$Neighbours]->[$NEIGH_TOP]
                || $self->get_neighbour($sqt, $NEIGH_TOP);
        $sqt->[$Row]->[0] = ($n->[$Row]->[$TILE_SIZE_CORE] & $BM_MIDDLE)
                | ($sqt->[$Row]->[0] & $BM_OUTER);
        $sqt->[$Row]->[1] = ($n->[$Row]->[$TILE_SIZE_CORE_P1] & $BM_MIDDLE)
                | ($sqt->[$Row]->[1] & $BM_OUTER);
    }
    if ($sqt->[$Updateflags] & (1 << $NEIGH_TOP_LEFT)) {
        my $n = $sqt->[$Neighbours]->[$NEIGH_TOP_LEFT]
                || $self->get_neighbour($sqt, $NEIGH_TOP_LEFT);
        $sqt->[$Row]->[0] = (
            (($n->[$Row]->[$TILE_SIZE_CORE] & $BM_MIDDLE) << $TILE_SIZE_CORE)
                     | ($sqt->[$Row]->[0] & $BM_RIGHT)
        );
        $sqt->[$Row]->[1] = (
            (($n->[$Row]->[$TILE_SIZE_CORE_P1] & $BM_MIDDLE) << $TILE_SIZE_CORE)
                     | ($sqt->[$Row]->[1] & $BM_RIGHT)
        );
    }
    if ($sqt->[$Updateflags] & (1 << $NEIGH_TOP_RIGHT)) {
        my $n = $sqt->[$Neighbours]->[$NEIGH_TOP_RIGHT]
                || $self->get_neighbour($sqt, $NEIGH_TOP_RIGHT);
        $sqt->[$Row]->[0] = (
            (($n->[$Row]->[$TILE_SIZE_CORE] & $BM_MIDDLE) >> $TILE_SIZE_CORE)
                     | ($sqt->[$Row]->[0] & $BM_LEFT)
        );
        $sqt->[$Row]->[1] = (
            (($n->[$Row]->[$TILE_SIZE_CORE_P1] & $BM_MIDDLE) >> $TILE_SIZE_CORE)
                     | ($sqt->[$Row]->[1] & $BM_LEFT)
        );
    }
    if ($sqt->[$Updateflags] & (1 << $NEIGH_BOTTOM)) {
        my $n = $sqt->[$Neighbours]->[$NEIGH_BOTTOM]
                || $self->get_neighbour($sqt, $NEIGH_BOTTOM);
        $sqt->[$Row]->[$TILE_SIZE_MBD] = (
            ($n->[$Row]->[$BORDER_WIDTH] & $BM_MIDDLE)
                     | ($sqt->[$Row]->[$TILE_SIZE_MBD] & $BM_OUTER)
        );
        $sqt->[$Row]->[$TILE_SIZE_FULL_M1] = (
            ($n->[$Row]->[3] & $BM_MIDDLE)
                     | ($sqt->[$Row]->[$TILE_SIZE_FULL_M1] & $BM_OUTER)
        );
    }
    if ($sqt->[$Updateflags] & (1 << $NEIGH_BOTTOM_LEFT)) {
        my $n = $sqt->[$Neighbours]->[$NEIGH_BOTTOM_LEFT]
                || $self->get_neighbour($sqt, $NEIGH_BOTTOM_LEFT);
        $sqt->[$Row]->[$TILE_SIZE_MBD] = (
            (($n->[$Row]->[$BORDER_WIDTH] & $BM_MIDDLE) << $TILE_SIZE_CORE)
                     | ($sqt->[$Row]->[$TILE_SIZE_MBD] & $BM_RIGHT)
        );
        $sqt->[$Row]->[$TILE_SIZE_FULL_M1] = (
            (($n->[$Row]->[3] & $BM_MIDDLE) << $TILE_SIZE_CORE)
                     | ($sqt->[$Row]->[$TILE_SIZE_FULL_M1] & $BM_RIGHT)
        );
    }
    if ($sqt->[$Updateflags] & (1 << $NEIGH_BOTTOM_RIGHT)) {
        my $n = $sqt->[$Neighbours]->[$NEIGH_BOTTOM_RIGHT]
                || $self->get_neighbour($sqt, $NEIGH_BOTTOM_RIGHT);
        $sqt->[$Row]->[$TILE_SIZE_MBD] = (
            (($n->[$Row]->[$BORDER_WIDTH] & $BM_MIDDLE) >> $TILE_SIZE_CORE)
                     | ($sqt->[$Row]->[$TILE_SIZE_MBD] & $BM_LEFT)
        );
        $sqt->[$Row]->[$TILE_SIZE_FULL_M1] = (
            (($n->[$Row]->[3] & $BM_MIDDLE) >> $TILE_SIZE_CORE)
                     | ($sqt->[$Row]->[$TILE_SIZE_FULL_M1] & $BM_LEFT)
        );
    }
    if ($sqt->[$Updateflags] & (1 << $NEIGH_LEFT)) {
        my $n = $sqt->[$Neighbours]->[$NEIGH_LEFT]
                || $self->get_neighbour($sqt, $NEIGH_LEFT);
        for my $i ($BORDER_WIDTH .. $TILE_SIZE_MBD_M1) {
            $sqt->[$Row]->[$i] = (
                (($n->[$Row]->[$i] & $BM_MIDDLE) << $TILE_SIZE_CORE)
                          | ($sqt->[$Row]->[$i] & $BM_RIGHT)
            );
        }
    }
    if ($sqt->[$Updateflags] & (1 << $NEIGH_RIGHT)) {
        my $n = $sqt->[$Neighbours]->[$NEIGH_RIGHT]
                || $self->get_neighbour($sqt, $NEIGH_RIGHT);
        for my $i ($BORDER_WIDTH .. $TILE_SIZE_MBD_M1) {
            $sqt->[$Row]->[$i] = (
                (($n->[$Row]->[$i] & $BM_MIDDLE) >> $TILE_SIZE_CORE)
                          | ($sqt->[$Row]->[$i] & $BM_LEFT)
            );
        }
    }
    return;
}

sub tick {
    my $self = shift;
    my $modified = $self->{Modified};

    # Update boundary of all modified tiles
    for my $sqt (@{$modified}) {
        $self->update_boundary($sqt) if $sqt->[$Updateflags] & $NEIGH_ANY;
        $sqt->[$Updateflags] = 0;
    }

    # Update core of all modified tiles, creating new modified list
    # Using map rather than foreach is odd, but it's faster (no need for
    # temporary list)
    @$modified = map {
        my $sqt = $_;
        my @tmp;
        my $row = $sqt->[$Row];
        my $top = my $bottom = $TILE_SIZE_FULL;
        $row->[$_] and $top = $_, last for 0..$TILE_SIZE_FULL_M1;
        if ($top != $TILE_SIZE_FULL) {
            1 while --$bottom && !$row->[$bottom];
            my($update_flag, $neigh) = st_tiletick($row, $top, $bottom);
            # update tile
            if ($update_flag) {
                push @tmp, $sqt if $sqt->[$Updateflags] == 0;
                $sqt->[$Updateflags] |= 1 << $NUM_NEIGH;
            }
            if ($neigh) {
                for my $i (0 .. $NUM_NEIGH_M1) {
                    next unless $neigh & (1 << $i);
                    # $self->update_neighbour($sqt, $i)
                    my $n = $sqt->[$Neighbours]->[$i]
                            || $self->get_neighbour($sqt, $i);
                    push @tmp, $n if $n->[$Updateflags] == 0;
                    $n->[$Updateflags] |= 1 << ($i ^ 4);
                }
            }
        }
        @tmp
    } @{$modified};
    return;
}

sub updatecell {
    my $self = shift;
    my $sqt = shift;
    my $ix = shift;
    my $iy = shift;

    push @{$self->{Modified}}, $sqt if $sqt->[$Updateflags] == 0;
    $sqt->[$Updateflags] |= 1 << $NUM_NEIGH;
    $self->update_neighbour($sqt, $NEIGH_TOP) if $iy <= $BORDER_WIDTH_P1;
    $self->update_neighbour($sqt, $NEIGH_BOTTOM) if $iy >= $TILE_SIZE_CORE;
    if ($ix <= $BORDER_WIDTH_P1) {
        $self->update_neighbour($sqt, $NEIGH_LEFT);
        $self->update_neighbour($sqt, $NEIGH_TOP_LEFT)
                if $iy <= $BORDER_WIDTH_P1;
        $self->update_neighbour($sqt, $NEIGH_BOTTOM_LEFT)
                if $iy >= $TILE_SIZE_CORE;
    }
    if ($ix >= $TILE_SIZE_CORE) {
        $self->update_neighbour($sqt, $NEIGH_RIGHT);
        $self->update_neighbour($sqt, $NEIGH_TOP_RIGHT)
                if $iy <= $BORDER_WIDTH_P1;
        $self->update_neighbour($sqt, $NEIGH_BOTTOM_RIGHT)
                if $iy >= $TILE_SIZE_CORE;
    }
    return;
}

1;
