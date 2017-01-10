package com.gmail.nuclearcat1337.horse_stats;

import com.mojang.realmsclient.gui.ChatFormatting;

import java.text.DecimalFormat;

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

    public String format(DecimalFormat format, double value)
    {
        String returnValue = format.format(value);

        if (value > getGood())
            returnValue = ChatFormatting.AQUA + returnValue + ChatFormatting.WHITE;
        else if (value > getAverage())
            returnValue = ChatFormatting.GREEN + returnValue + ChatFormatting.WHITE;
        else if (value < getBad())
            returnValue = ChatFormatting.RED + returnValue + ChatFormatting.WHITE;

        return returnValue;
    }

    @Override
    public String toString()
    {
        return new StringBuilder().append(getBad()).append('-').append(getAverage()).append('-').append(getGood()).toString();
    }

    public static Threshold fromString(String stringForm)
    {
        String[] args = stringForm.split("-");
        if(args.length != 3)
            return null;

        try
        {
            double bad = Double.parseDouble(args[0]);
            double average = Double.parseDouble(args[1]);
            double good = Double.parseDouble(args[2]);

            return new Threshold(bad,average,good);
        }
        catch(NumberFormatException e)
        {
            return null;
        }
    }
}
