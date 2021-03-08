package TL::View;
use strict;
use warnings;
use Tk;
use Tk::After;
use Tk::Adjuster;

sub new {
    my($class, $world) = @_;
    my $self = bless {
        world => $world,
    }, $class;
    # trigger window creation
    $self->main_window;
    $self->reset;
    return $self;
}
sub world { $_[0]->{world} }
sub centre { $_[0]->{centre} }
sub step_size { $_[0]->{step_size} }
sub selected { $_[0]->{selected} }
sub font_size { $_[0]->{font_size} }

sub reset {
    my($self) = @_;
    $self->{centre} = [0, 0];
    $self->{step_size} = 1;
    $self->{font_size} = [8, 8];    # assumed for now
    $self->select_life(0, 0);
    $self->render_world;
    $self->render_life;
    return;
}

sub main_window {
    my($self) = @_;
    return $self->{main_window} // do {
        my $mw = $self->{main_window} = Tk::MainWindow->new;
        $mw->configure(-title => "Turing/Life");
        my $tc = $self->top_container;

        $tc->pack(
            -side => 'top',
            -expand => 'yes',
            -fill => 'both',
            -padx => '2m',
            -pady => '2',
        );

        $mw;
    };
}

sub top_container {
    my($self) = @_;
    return $self->{top_container} // do {
        my $tc = $self->{top_container} = $self->main_window->Frame;
        my $wc = $self->world_container;
        my $cv = $self->control_view;

        # As Panedwindow, but with extra space going to top window
        $cv->pack(
            -side => 'bottom',
            -fill => 'x',
            -expand => 0,
        );
        $tc->Adjuster->packAfter($cv, -side => 'bottom');
        $wc->pack(
            -side => 'top',
            -expand => 'yes',
            -fill => 'both',
            -padx => '2',
            -pady => '2m',
        );

        $tc;
    };
}

sub world_container {
    my($self) = @_;
    return $self->{world_container} // do {
        my $wc = $self->{world_container} = $self->top_container->Frame;
        my $wv = $self->world_view;
        my $lv = $self->life_view;

        # As Panedwindow, but with extra space going to left window
        $lv->pack(
            -side => 'right',
            -fill => 'y',
            -expand => 0,
        );
        $wc->Adjuster->packAfter($lv, -side => 'right');
        $wv->pack(
            -side => 'left',
            -fill => 'both',
            -expand => 1,
        );

        $wc;
    };
}

sub world_view {
    my($self) = @_;
    return $self->{world_view} // do {
        my $wv = $self->{world_view} = $self->world_container->Canvas;

        # Allow click to select, and drag to move where we're looking.
        $wv->CanvasBind('<1>', [ sub {
            my($window, $x, $y) = @_;
            $self->{b1_pressed} = [$x, $y];
            return;
        }, Tk::Ev('x'), Tk::Ev('y') ]);
        $wv->CanvasBind('<B1-ButtonRelease>', [ sub {
            my($window, $x, $y) = @_;
            my($ox, $oy) = @{ delete $self->{b1_pressed} // return };
            my($fx, $fy) = @{ $self->font_size };
            if (abs($x - $ox) < $fx && abs($y - $oy) < $fy) {
                $self->select_life($self->p2c($x, $y));
            } else {
                $self->move_centre($x - $ox, $y - $oy);
            }
            return;
        }, Tk::Ev('x'), Tk::Ev('y') ]);

        $wv;
    };
}

sub life_view {
    my($self) = @_;
    return $self->{life_view} // do {
        my $lv = $self->{life_view}
                = $self->world_container->Frame(-width => 100);

        for my $v (qw{ steps_view x_view y_view cell_view energy_view }) {
            my $w = $self->$v();
            $w->pack(
                -side => 'top',
                -anchor => 'n',
                -expand => 'no',
            );
        }
        $self->org_view->pack(
            -side => 'top',
            -anchor => 'n',
            -expand => 'both',
        );

        $lv;
    };
}

sub steps_view {
    my($self) = @_;
    return $self->{count_view}
            //= TL::View::tl_Value->instance($self->life_view, 'steps');
}
sub x_view {
    my($self) = @_;
    return $self->{x_view}
            //= TL::View::tl_Value->instance($self->life_view, 'x');
}
sub y_view {
    my($self) = @_;
    return $self->{y_view}
            //= TL::View::tl_Value->instance($self->life_view, 'y');
}
sub cell_view {
    my($self) = @_;
    return $self->{cell_view}
            //= TL::View::tl_Value->instance($self->life_view, 'cell');
}
sub energy_view {
    my($self) = @_;
    return $self->{energy_view}
            //= TL::View::tl_Value->instance($self->life_view, 'energy');
}

sub org_view {
    my($self) = @_;
    return $self->{org_view} // do {
        my $ov = $self->{org_view} = $self->life_view->Frame;
        $self->{ov_org} = undef;
        $self->{ov_grid} = [];
        $self->{ov_cur} = undef;

        $ov->Label(-text => '0')->grid(-column => 1, -row => 0);
        $ov->Label(-text => '1')->grid(-column => 2, -row => 0);

        $ov;
    };
}

sub set_org {
    my($self, $new, $state) = @_;
    my($old, $grid, $ocur) = @$self{qw{ ov_org ov_grid ov_cur }};
    $self->{ov_org} = $new;
    my $ov = $self->org_view;
    if (defined $new) {
        my $rules = $new->genome;
        my $rule = $new->state;
        unless (defined($old) && $old == $new) {
            $_->destroy for splice @$grid, 0;
            for my $r (0 .. $#$rules) {
                push @$grid, $ov->Label(
                    -text => $r,
                )->grid(
                    -column => 0,
                    -row => $r + 1,
                );
                for my $s (0, 1) {
                    push @$grid, $ov->Label(
                        -text => $new->rulestr($rules->[$r][$s]),
                        -bg => 'SlateGray1',
                    )->grid(
                        -column => $s + 1,
                        -row => $r + 1,
                    );
                }
            }
        } elsif (defined $ocur) {
            $grid->[$ocur]->configure(-bg => 'SlateGray1');
        }
        if ($new->dead) {
            $self->{ov_cur} = undef;
        } else {
            $self->{ov_cur} = 3 * $new->state + $state + 1;
            $grid->[$self->{ov_cur}]->configure(-bg => 'PaleGreen1');
        }
    } elsif (defined $old) {
        $_->destroy for splice @$grid, 0;
        $self->{ov_cur} = undef;
    }
    return;
}

sub control_view {
    my($self) = @_;
    return $self->{control_view} // do {
        my $cv = $self->{control_view} = $self->top_container->Frame;
        $self->cv_top->pack(
            -side => 'top',
            -fill => 'x',
        );
        $self->cv_bottom->pack(
            -side => 'bottom',
            -fill => 'x',
        );

        $cv;
    };
}

sub cv_top {
    my($self) = @_;
    return $self->{cv_top} // do {
        my $cvtop = $self->{cv_top} = $self->control_view->Frame;
        my @pack = (-expand => 'no', -pady => 2);

        $self->step_button->pack(-side => 'left', @pack);
        $self->run_button->pack(-side => 'left', @pack);
        $self->stop_button->pack(-side => 'left', @pack);

        $self->reset_button->pack(-side => 'right', @pack);
        $self->seed_entry->pack(-side => 'right', @pack);
        $cvtop->Label(-text => 'Seed:')->pack(-side => 'right', @pack);

        $cvtop;
    };
}

sub cv_bottom {
    my($self) = @_;
    return $self->{cv_bottom} // do {
        my $cvbottom = $self->{cv_bottom} = $self->control_view->Frame;

        for my $method (qw{ quit_button }) {
            my $w = $self->$method();
            $w->pack(
                -side => 'right',
                -expand => 'no',
                -pady => 2,
            );
        }

        $cvbottom;
    };
}

sub step_button {
    my($self) = @_;
    return $self->{step_button} //= $self->cv_top->Button(
        -text => 'Step',
        -command => sub { $self->step },
    );
}

sub run_button {
    my($self) = @_;
    return $self->{run_button} //= $self->cv_top->Button(
        -text => 'Run',
        -command => sub {
            $self->run_button->configure(-state => 'disabled');
            $self->stop_button->configure(-state => 'normal');
            $self->{run_action} = Tk::After->new(
                $self->top_container, 'idle', 'repeat', sub { $self->step },
            );
            return;
        },
    );
}

sub stop_button {
    my($self) = @_;
    return $self->{stop_button} //= $self->cv_top->Button(
        -text => 'Stop',
        -state => 'disabled',
        -command => sub {
            $self->run_button->configure(-state => 'normal');
            $self->stop_button->configure(-state => 'disabled');
            $self->{run_action}->cancel;
            delete $self->{run_action};
            return;
        },
    );
}

sub seed_entry {
    my($self) = @_;
    return $self->{seed_entry} //= do {
        my $se = $self->cv_top->Spinbox(
            -from => 1,
            -increment => 1,
            -to => ~0,
            -width => 5,
        );
        $se->set($self->world->seed);
        $se;
    };
}

sub reset_button {
    my($self) = @_;
    return $self->{reset_button} //= $self->cv_top->Button(
        -text => 'Reset',
        -command => sub {
            $self->world->reset({ seed => $self->seed_entry->get });
            $self->reset;
            return;
        },
    );
}

sub quit_button {
    my($self) = @_;
    return $self->{quit_button} //= $self->cv_bottom->Button(
        -text => 'Quit',
        -command => sub { $self->main_window->destroy },
    );
}

sub step {
    my($self) = @_;
    my $size = $self->step_size;
    $self->world->step($size);
    $self->render_world;
    $self->render_life;
    return;
}

sub _floor {
    my $x = int($_[0]);
    $_[0] < 0 && $x != $_[0] ? $x - 1 : $x;
}

sub _round { _floor($_[0] + 0.5) }

sub p2c {
    my($self, $px, $py) = @_;
    my $w = $self->world_view;
    my($wx, $wy) = (_floor($w->width / 2), _floor($w->height / 2));
    my($fx, $fy) = @{ $self->font_size };
    my($cx, $cy) = @{ $self->centre };
    return +(
        _floor(($px - $wx) / $fx) - $cx,
        _floor(($py - $wy) / $fy) - $cy,
    );
}

sub move_centre {
    my($self, $x, $y) = @_;
    my($fx, $fy) = @{ $self->font_size };
    my $c = $self->centre;
    $c->[0] += _round($x / $fx);
    $c->[1] += _round($y / $fy);
    Tk::After->new($self->top_container, 'idle', 'once',
            sub { $self->render_world });
    return;
}

sub render_world {
    my($self) = @_;
    my $w = $self->world_view;
    $w->delete($w->find(withtag => 'all'));

    my $world = $self->world;
    my($wx, $wy) = ($w->width, $w->height);
    my($fx, $fy) = @{ $self->font_size };
    for (my $py = 0; $py < $wy; $py += $fy) {
        for (my $px = 0; $px < $wx; $px += $fx) {
            my($x, $y) = $self->p2c($px, $py);
            my($state, $org) = $world->read_cell($x, $y);
            my $char = $org ? ['>', 'v', '<', '^']->[ $org->facing ]
                    : $state ? 'o' : ' ';
            $w->createText($px, $py,
                -anchor => 'w',
                -tags => ['cell'],
                -text => $char,
            );
        }
    }
    return;
}

sub render_life {
    my($self) = @_;
    my $world = $self->world;
    my($loc, $org) = @{ $self->selected };
    my $cell;

    $loc = $org->location if $org && !$org->dead;
    $cell = $world->world->read_cell(@$loc) if $loc;

    $self->steps_view->set_value($world->step_count);

    $self->x_view->set_value($loc ? $loc->[0] : undef);
    $self->y_view->set_value($loc ? $loc->[1] : undef);
    $self->cell_view->set_value($loc ? $cell : undef);

    $self->energy_view->set_value(
        $org ? $org->dead ? 'dead' : $org->energy : undef
    );
    $self->set_org($org, $cell);
    return;
}

sub select_life {
    my($self, $x, $y) = @_;
    my($state, $org) = $self->world->read_cell($x, $y);
    if ($org) {
        $self->{selected} = [ undef, $org ];
    } else {
        $self->{selected} = [ [ $x, $y ], undef ];
    }
    $self->render_life;
    return;
}

package TL::View::tl_Value {
    use parent qw{ Tk::Derived Tk::Label };
    Tk::Widget->Construct('tl_Value');
    sub instance {
        my($class, $parent, $legend) = @_;
        my $self = $parent->tl_Value(-text => "$legend:");
        $self->{tl_legend} = $legend;
        $self->{tl_value} = undef;
        return $self;
    }
    sub set_value {
        my($self, $new) = @_;
        my $old = $self->{tl_value};
        if (defined $new) {
            return if defined($old) && $new eq $old;
            $self->configure(-text => "$self->{tl_legend}: $new");
        } else {
            return if !defined($old);
            $self->configure(-text => "$self->{tl_legend}:");
        }
        $self->{tl_value} = $new;
        return;
    }
};

1;
