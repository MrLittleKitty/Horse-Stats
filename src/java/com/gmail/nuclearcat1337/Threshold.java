package com.gmail.nuclearcat1337;

/*
Created by Mr_Little_Kitty on 12/22/2015
*/
public class Threshold
{
    private double bad,average,good;
    public Threshold(double bad, double average, double good)
    {
        this.bad = bad;
        this.average = average;
        this.good = good;
    }

    public double getBad()
    {
        return bad;
    }

    public double getAverage()
    {
        return average;
    }

    public double getGood()
    {
        return good;
    }

    public void setBad(final double bad)
    {
        this.bad = bad;
    }

    public void setAverage(final double average)
    {
        this.average = average;
    }

    public void setGood(final double good)
    {
        this.good = good;
    }
}
