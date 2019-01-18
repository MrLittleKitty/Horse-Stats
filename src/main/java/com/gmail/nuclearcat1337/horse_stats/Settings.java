package com.gmail.nuclearcat1337.horse_stats;

import com.gmail.nuclearcat1337.horse_stats.Threshold;

public class Settings {

    public Threshold jumpThreshold = new Threshold(4, 5);
    public Threshold speedThreshold = new Threshold(11, 13);
    public Threshold healthThreshold = new Threshold(24, 28);

    public float renderDistance = 15.0F;
    public boolean shouldRender = Boolean.TRUE;
    public int decimalPlaces = 3;
}
