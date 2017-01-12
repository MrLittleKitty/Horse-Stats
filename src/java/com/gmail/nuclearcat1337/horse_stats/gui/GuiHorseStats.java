package com.gmail.nuclearcat1337.horse_stats.gui;

import com.gmail.nuclearcat1337.horse_stats.HorseStats;
import com.gmail.nuclearcat1337.horse_stats.Threshold;
import net.minecraft.client.gui.*;

import java.text.DecimalFormat;

/**
 * Created by Mr_Little_Kitty on 1/10/2017.
 */
public class GuiHorseStats extends GuiScreen
{
    private static final int BUTTON_WIDTH = GuiConstants.MEDIUM_BUTTON_WIDTH;

    private final HorseStats horseStats;
    public GuiHorseStats(HorseStats stats)
    {
        this.horseStats = stats;
    }

    @Override
    public void initGui()
    {
        this.buttonList.clear();

        int xPos = (this.width/2) - (BUTTON_WIDTH/2) - (GuiConstants.STANDARD_SEPARATION_DISTANCE*2) - BUTTON_WIDTH;
        int yPos = (this.height/2) - (GuiConstants.STANDARD_BUTTON_HEIGHT/2) - GuiConstants.STANDARD_SEPARATION_DISTANCE - GuiConstants.STANDARD_BUTTON_HEIGHT;

        layoutThresholdButtons(xPos,yPos, horseStats.getJumpThreshold(),"Jump",1,6);

        xPos += BUTTON_WIDTH + GuiConstants.STANDARD_SEPARATION_DISTANCE*2;

        layoutThresholdButtons(xPos,yPos, horseStats.getHealthThreshold(),"Health",18,32);

        xPos += BUTTON_WIDTH + GuiConstants.STANDARD_SEPARATION_DISTANCE*2;

        layoutThresholdButtons(xPos,yPos, horseStats.getSpeedThreshold(),"Speed",8,15);

        super.initGui();
    }

    @Override
    public void actionPerformed(GuiButton button)
    {
        if(!button.enabled)
            return;


    }

    @Override
    public boolean doesGuiPauseGame()
    {
        return false;
    }

    private static final DecimalFormat format = new DecimalFormat("#.0");
    private static final GuiSlider.FormatHelper formatHelper = new GuiSlider.FormatHelper()
    {
        @Override
        public String getText(int id, String name, float value)
        {
            return name +": "+format.format(value);
        }
    };

    private void layoutThresholdButtons(int xPos, int yPos, Threshold threshold, String name, float min, float max)
    {
        GuiSlider goodSlider = new GuiSlider(new ThresholdRunnable(threshold,0),1,xPos,yPos,name+" Good",min,max,threshold.getGood(),formatHelper);
        goodSlider.width = BUTTON_WIDTH;
        //GuiSlider goodSlider = new GuiSlider(1,xPos,yPos,BUTTON_WIDTH,GuiConstants.STANDARD_BUTTON_HEIGHT,name+" Good",threshold.getGood(), min,max, valueStep, new ThresholdRunnable(threshold,0));

        yPos += goodSlider.height + GuiConstants.STANDARD_SEPARATION_DISTANCE;

        GuiSlider averageSlider = new GuiSlider(new ThresholdRunnable(threshold,1),2,xPos,yPos,name+" Avg",min,max,threshold.getAverage(),formatHelper);
        averageSlider.width = BUTTON_WIDTH;
        //GuiSlider averageSlider = new GuiSlider(2,xPos,yPos,BUTTON_WIDTH,GuiConstants.STANDARD_BUTTON_HEIGHT,name+" Average", threshold.getAverage(), min,max, valueStep, new ThresholdRunnable(threshold,1));

        yPos += averageSlider.height + GuiConstants.STANDARD_SEPARATION_DISTANCE;

        GuiSlider badSlider = new GuiSlider(new ThresholdRunnable(threshold,2),3,xPos,yPos,name+" Bad",min,max,threshold.getBad(),formatHelper);
        badSlider.width = BUTTON_WIDTH;
        //GuiSlider badSlider = new GuiSlider(3,xPos,yPos,BUTTON_WIDTH,GuiConstants.STANDARD_BUTTON_HEIGHT,name+" Bad",threshold.getBad(), min,max,valueStep, new ThresholdRunnable(threshold,2));

        buttonList.add(goodSlider);
        buttonList.add(averageSlider);
        buttonList.add(badSlider);
    }

    private static class ThresholdRunnable implements GuiPageButtonList.GuiResponder
    {
        private final Threshold threshold;
        private final int value;

        public ThresholdRunnable(Threshold threshold, int value)
        {
            this.threshold = threshold;
            this.value = value;
        }

        @Override
        public void setEntryValue(int id, boolean value)
        {

        }

        @Override
        public void setEntryValue(int id, float value)
        {
            if(this.value == 0)
                threshold.setGood(value);
            else if(this.value == 1)
                threshold.setAverage(value);
            else
                threshold.setBad(value);
        }

        @Override
        public void setEntryValue(int id, String value)
        {

        }
    }
}
