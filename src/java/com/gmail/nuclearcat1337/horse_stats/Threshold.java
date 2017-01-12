package com.gmail.nuclearcat1337.horse_stats;

import com.mojang.realmsclient.gui.ChatFormatting;

import java.text.DecimalFormat;

/*
Created by Mr_Little_Kitty on 12/22/2015
*/
public class Threshold
{
    private float average,great;
    public Threshold(float average, float great)
    {
        this.average = average;
        this.great = great;
    }

    public float getAverage()
    {
        return average;
    }

    public float getGreat()
    {
        return great;
    }

    public void setAverage(final float average)
    {
        this.average = average;
    }

    public void setGreat(final float great)
    {
        this.great = great;
    }

    public String format(DecimalFormat format, float value)
    {
        String returnValue = format.format(value);

        if (value >= getGreat()) //If its higher than the "great" point then we color it
            returnValue = ChatFormatting.AQUA + returnValue + ChatFormatting.WHITE;
        else if (value >= getAverage()) //If its not higher than the "great" point but its above "average", color it
            returnValue = ChatFormatting.GREEN + returnValue + ChatFormatting.WHITE;
        else //If its below the "average" then color it for "bad"
            returnValue = ChatFormatting.RED + returnValue + ChatFormatting.WHITE;

        return returnValue;
    }

    @Override
    public String toString()
    {
        return new StringBuilder().append(getAverage()).append('-').append(getGreat()).toString();
    }

    public static Threshold fromString(String stringForm)
    {
        String[] args = stringForm.split("-");
        if(args.length != 2)
            return null;

        try
        {
            float average = Float.parseFloat(args[0]);
            float great = Float.parseFloat(args[1]);

            return new Threshold(average,great);
        }
        catch(NumberFormatException e)
        {
            return null;
        }
    }
}
