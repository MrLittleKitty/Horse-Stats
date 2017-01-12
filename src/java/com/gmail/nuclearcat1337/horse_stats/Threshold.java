package com.gmail.nuclearcat1337.horse_stats;

import com.mojang.realmsclient.gui.ChatFormatting;

import java.text.DecimalFormat;

/*
Created by Mr_Little_Kitty on 12/22/2015
*/
public class Threshold
{
    private float bad,average,good;
    public Threshold(float bad, float average, float good)
    {
        this.bad = bad;
        this.average = average;
        this.good = good;
    }

    public float getBad()
    {
        return bad;
    }

    public float getAverage()
    {
        return average;
    }

    public float getGood()
    {
        return good;
    }

    public void setBad(final float bad)
    {
        this.bad = bad;
    }

    public void setAverage(final float average)
    {
        this.average = average;
    }

    public void setGood(final float good)
    {
        this.good = good;
    }

    public String format(DecimalFormat format, float value)
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
            float bad = Float.parseFloat(args[0]);
            float average = Float.parseFloat(args[1]);
            float good = Float.parseFloat(args[2]);

            return new Threshold(bad,average,good);
        }
        catch(NumberFormatException e)
        {
            return null;
        }
    }
}
