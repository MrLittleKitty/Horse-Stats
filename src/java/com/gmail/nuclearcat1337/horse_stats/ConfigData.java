package com.gmail.nuclearcat1337.horse_stats;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;

/*
Created by Mr_Little_Kitty on 12/22/2015
*/
public class ConfigData
{
    private Threshold speed, jump, health;
    private int renderDistance;
    private int maxNumberOfOverlays;
    private boolean showBackground;
    private int numberOfDecimals;

    private File targetFile;

    public ConfigData(String filePath)
    {
        targetFile = new File(filePath);
        if(!targetFile.exists())
        {
            createDefaultFile();
        }
        loadData();
    }

    private void createDefaultFile()
    {
        try
        {
            targetFile.createNewFile();
            FileWriter fileWriter = new FileWriter(targetFile);
            BufferedWriter writer = new BufferedWriter(fileWriter);

            writer.write("jump-bad=2.5");
            writer.newLine();
            writer.write("jump-average=4");
            writer.newLine();
            writer.write("jump-good=5");
            writer.newLine();

            writer.write("speed-bad=9.5");
            writer.newLine();
            writer.write("speed-average=11");
            writer.newLine();
            writer.write("speed-good=13");
            writer.newLine();

            writer.write("health-bad=20");
            writer.newLine();
            writer.write("health-average=24");
            writer.newLine();
            writer.write("health-good=28");
            writer.newLine();

            writer.write("number-of-decimals=2");
            writer.newLine();

            writer.flush();
            writer.close();
        }
        catch (IOException e)
        {
            e.printStackTrace();
        }

    }

    public void loadData()
    {
        loadDefaults();
        if(targetFile.exists())
        {
            try
            {
                FileReader fileReader = new FileReader(targetFile);
                BufferedReader reader = new BufferedReader(fileReader);
                String line;
                while((line = reader.readLine()) != null)
                {
                    String[] lines = line.split("=");
                    if(lines.length == 2)
                        setValue(lines[0],lines[1]);
                }
                reader.close();
                fileReader.close();
            }
            catch(IOException ex)
            {
                ex.printStackTrace();
            }
        }
    }

    private void setValue(String key, final String value)
    {
        key = key.toLowerCase();

        if(key.contains("jump"))
            setThresholdValue(jump,key,value);
        else if(key.contains("speed"))
            setThresholdValue(speed,key,value);
        else if (key.contains("health"))
            setThresholdValue(health,key,value);
        else if(key.equals("render-distance"))
            renderDistance = Integer.parseInt(value);
        else if(key.equals("number-of-decimals"))
            numberOfDecimals = Integer.parseInt(value);
    }

    private void setThresholdValue(Threshold hold, String key, String value)
    {
        if(key.contains("bad"))
            hold.setBad(Double.parseDouble(value));
        else if(key.contains("average"))
            hold.setAverage(Double.parseDouble(value));
        else if(key.contains("good"))
            hold.setGood(Double.parseDouble(value));
    }

    public Threshold getSpeedThreshold()
    {
        return speed;
    }

    public Threshold getJumpThreshold()
    {
        return jump;
    }

    public Threshold getHealthThreshold()
    {
        return health;
    }

    public int getRenderDistance()
    {
        return renderDistance;
    }

    public int getRenderDistanceSquared()
    {
        return renderDistance*renderDistance;
    }

    public int getMaxNumberOfOverlays()
    {
        return maxNumberOfOverlays;
    }

    public boolean showBackground()
    {
        return showBackground;
    }

    public int getNumberOfDecimals()
    {
        return numberOfDecimals;
    }

    public void loadDefaults()
    {
        speed = new Threshold(9.5,11,13);
        jump = new Threshold(2.5,4,5);
        health = new Threshold(20,24,28);
        renderDistance = 8;
        maxNumberOfOverlays = 75;
        showBackground = true;
        numberOfDecimals = 2;
    }
}
