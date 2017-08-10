package com.gmail.nuclearcat1337.horse_stats;

import net.minecraft.entity.EntityLivingBase;
import net.minecraft.entity.SharedMonsterAttributes;
import net.minecraft.entity.passive.EntityHorse;

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
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
    public static DecimalFormat createDecimalFormat(int numberOfDecimalsDisplayed)
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
    public static float getHorseMaxJump(EntityHorse horse)
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
    public static float getEntityMaxSpeed(EntityLivingBase entity)
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
    public static int getEntityMaxHP(EntityLivingBase entity)
    {
        return (int) entity.getEntityAttribute(SharedMonsterAttributes.MAX_HEALTH).getAttributeValue();
    }

    /**
     * Gets the baby horses age ranging from 0 to 100.
     * @param horse
     * @return
     */
    public static int getHorseBabyGrowingAgeAsPercent(EntityHorse horse)
    {
        float horseGrowingAge = horse.getHorseSize();	//horse size ranges from 0.5 to 1
        return (int)((horseGrowingAge - 0.5f) * 2.0f * 100f);
    }
    
    /**
     * Gets a horses  coloring
     *
     * @param horse
     * @return empty string if there is no coloring
     */
    public static String getHorseMarkingText(EntityHorse horse) {
            return Arrays.stream(horse.getVariantTexturePaths())
                .filter(s->s != null)
                .map(s -> s.split("[\\p{Punct}\\s]+"))
                .filter(a -> a.length > 2)
                .map(a -> a[a.length - 2])
                .reduce((a, b) -> String.join(" ", b, a))
                .map(WordUtils::capitalizeFully)
                .orElse("");
    }

    public static String getCurrentHealth(EntityHorse horse) {
        return String.format("%.1f/", (float) ((int) (horse.getHealth() * 2)) / 2.0F);
    }

    public static String getHorseDetails(EntityHorse horse, Long childAge, Long sinceBreeding) {
        final List<String> details = new ArrayList<>();
        if (!horse.isChild()) {
            details.add(!horse.isTame() ? "Wild" : "Tamed");
        }
        details.add(horse.isChild() ? "Foal" : "Adult");
        details.add(WordUtils.capitalizeFully(horse.getType().toString()));
        if (sinceBreeding != null) {
            details.add("Bred " + ((System.currentTimeMillis() - sinceBreeding) / 1000) + "s");
        }
        if (childAge != null) {
            details.add("Age " + (System.currentTimeMillis() - childAge) / 1000 + "s");
        }
        details.addAll(horse.getTags());
        return String.join(" ", details.toArray(new String[]{}));
    }


}
