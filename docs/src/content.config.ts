import { defineCollection, z } from 'astro:content';

const docs = defineCollection({
  type: 'content',
  schema: z.object({
    title: z.string(),
    description: z.string().optional(),
    section: z.string().optional(),
    order: z.number().optional(),
    template: z.enum(['doc', 'landing']).default('doc'),
  }),
});

export const collections = { docs };
