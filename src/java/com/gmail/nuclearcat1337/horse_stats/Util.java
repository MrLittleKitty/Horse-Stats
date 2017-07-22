package com.gmail.nuclearcat1337.horse_stats;

import net.minecraft.entity.EntityLivingBase;
import net.minecraft.entity.SharedMonsterAttributes;
import net.minecraft.entity.passive.EntityHorse;

import java.text.DecimalFormat;
import org.apache.commons.lang3.text.WordUtils;

/**
 * Created by Mr_Little_Kitty on 1/10/2017.
 */
public class Util
{
    /**
     * Gets the amount of decimals that should be displayed with a DecimalFormat object.
     * @return
     */
    public static DecimalFormat CreateDecimalFormat(int numberOfDecimalsDisplayed)
    {
        if(numberOfDecimalsDisplayed < 1)
            return new DecimalFormat("#");

        String format = "#.";
        for(int i = 1; i <= numberOfDecimalsDisplayed; i++)
            format += "#";

        return new DecimalFormat(format);
    }

    /**
     * Gets the max height a horse can jump when the jump bar is fully charged.
     * @param horse
     * @return e.x. 1.2?-5.5?
     */
    public static float GetHorseMaxJump(EntityHorse horse)
    {
        //simulate gravity and air resistance to determine the jump height
        double yVelocity = horse.getHorseJumpStrength();	//horses's jump strength attribute
        double jumpHeight = 0;
        while (yVelocity > 0)
        {
            jumpHeight += yVelocity;
            yVelocity -= 0.08;
            yVelocity *= 0.98;
        }
        return (float)jumpHeight;
    }

    /**
     * Gets an entity's max run speed in meters(blocks) per second
     * @param entity
     * @return e.x. Steve = 4.3 m/s. Horses ~7-13
     */
    public static float GetEntityMaxSpeed(EntityLivingBase entity)
    {
        //Steve has a movement speed of 0.1 and walks 4.3 blocks per second,
        //so multiply this result by 43 to convert to blocks per second
        return (float)entity.getEntityAttribute(SharedMonsterAttributes.MOVEMENT_SPEED).getAttributeValue() * 43;
    }

    /**
     * Gets an entity's max hit points
     * @param entity
     * @return e.x. Steve = 20 hit points
     */
    public static int GetEntityMaxHP(EntityLivingBase entity)
    {
        return (int) entity.getEntityAttribute(SharedMonsterAttributes.MAX_HEALTH).getAttributeValue();
    }

    /**
     * Gets the baby horses age ranging from 0 to 100.
     * @param horse
     * @return
     */
    public static int GetHorseBabyGrowingAgeAsPercent(EntityHorse horse)
    {
        float horseGrowingAge = horse.getHorseSize();	//horse size ranges from 0.5 to 1
        return (int)((horseGrowingAge - 0.5f) * 2.0f * 100f);
    }
    
    /**
     * Gets a horses primary coloring
     *
     * @param horse
     * @return empty string if there is no coloring (for donkeys)
     */
    public static String GetHorseColoringText(EntityHorse horse) {
        String texture = "";
        if (horse instanceof EntityHorse) {
            texture = ((EntityHorse) horse).getVariantTexturePaths()[0];
        }

        if (texture == null || texture.isEmpty())
            return "";

        String[] textureArray = texture.split("/");            //"textures/entity/horse/horse_creamy.png"
        texture = textureArray[textureArray.length - 1];        //"horse_creamy.png"
        texture = texture.substring(6, texture.length() - 4);    //"creamy"
        texture = WordUtils.capitalize(texture);            //"Creamy"

        return texture;
    }

    /**
     * Gets a horses secondary coloring
     *
     * @param horse
     * @return empty string if there is no secondary coloring (for donkeys)
     */
    public static String GetHorseMarkingText(EntityHorse horse) {
        String texture = "";
        if (horse instanceof EntityHorse) {
            texture = ((EntityHorse) horse).getVariantTexturePaths()[1];
        }
        if (texture == null || texture.isEmpty())
            return "";

        String[] textureArray = texture.split("/");                //"textures/entity/horse/horse_markings_blackdots.png"
        texture = textureArray[textureArray.length - 1];            //"horse_markings_blackdots.png"
        texture = texture.substring(15, texture.length() - 4);    //"blackdots"
        texture = WordUtils.capitalize(texture);                //"Blackdots"

        return texture;
    }
}
